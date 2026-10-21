module LordOfTheRings.Attachment.Cards.MagicRing (magicRing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ring))

newtype MagicRing = MagicRing AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero and 1 per deck.
--
-- ponytail: the per hero limit is read as no Ring already attached, and the per deck
-- limit is deckbuilding's
magicRing :: AttachmentCard MagicRing
magicRing = attachment MagicRing Cards.magicRing & attachTo (AttachToHero $ not_ $ HeroWithAttached Ring)

-- | Action: Exhaust Magic Ring and raise your threat by 1 to (choose 1): heal 1 damage
-- from attached hero, add 1 resource to attached hero's pool, or ready attached hero.
instance HasAbilities MagicRing where
  getAbilities (MagicRing a) = [mkAbility a 1 $ ActionAbility (Costs [ExhaustCost $ toTarget a, RaiseThreatCost 1])]

instance RunMessage MagicRing where
  runMessage msg s@(MagicRing a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a \host -> chooseOneM pid do
        questionLabeled "magicRing.choose"
        causedBy a
        labeled "magicRing.heal" $ healDamage a host 1
        labeled "magicRing.resource" $ placeResources a host 1
        labeled "magicRing.ready" $ ready host
      pure s
    _ -> MagicRing <$> runMessage msg a
