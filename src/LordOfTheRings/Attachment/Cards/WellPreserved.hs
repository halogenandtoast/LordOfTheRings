module LordOfTheRings.Attachment.Cards.WellPreserved (wellPreserved) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype WellPreserved = WellPreserved AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the hero with The One Ring. Limit 1 per hero, on its card def.
wellPreserved :: AttachmentCard WellPreserved
wellPreserved = attachment WellPreserved Cards.wellPreserved & attachTo (AttachToHero heroWithTheOneRing)

-- | Attached hero gets +1 hit point.
instance HasModifiersFor WellPreserved where
  getModifiersFor (WellPreserved a) = withHost a \host -> modified a host [HitPointModifier 1]

-- | Response: At the beginning of the round, exhaust The One Ring and raise your threat by 1
-- to heal all damage from attached hero.
instance HasAbilities WellPreserved where
  getAbilities (WellPreserved a) =
    [mkAbility a 1 $ ResponseAbility RoundBeginsWindow theOneRingCost | AttachedTo (HeroTarget _) <- [a.placement]]

instance RunMessage WellPreserved where
  runMessage msg s@(WellPreserved a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a $ healAllDamage a
      pure s
    _ -> WellPreserved <$> runMessage msg a
