module LordOfTheRings.Attachment.Cards.Herugrim (herugrim) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Rohan))

newtype Herugrim = Herugrim AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Rohan hero. Restricted, on its card def.
herugrim :: AttachmentCard Herugrim
herugrim = attachment Herugrim Cards.herugrim & attachTo (AttachToHero $ HeroWithTrait Rohan)

-- | Reduce the cost to play Herugrim on Théoden by 1: given from hand, before it is played.
instance HasModifiersFor Herugrim where
  getModifiersFor (Herugrim a) = case a.placement of
    Unplaced -> do
      theodens <- select (HeroWithTitle "Théoden")
      for_ theodens \h -> modified a h [ReduceCostToAttach (CardWithCode $ toCardCode Cards.herugrim) 1]
    _ -> pure ()

-- | Response: After attached hero is declared as an attacker, exhaust Herugrim to add
-- attached hero's [willpower] to its [attack] for this attack.
instance HasAbilities Herugrim where
  getAbilities (Herugrim a) =
    [mkAbility a 1 $ ResponseAbility (AttackerDeclaredWindow After (Just host)) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage Herugrim where
  runMessage msg s@(Herugrim a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> do
        willpower <- getWillpower host
        attackModifier a host (AttackModifier willpower)
      pure s
    _ -> Herugrim <$> runMessage msg a
