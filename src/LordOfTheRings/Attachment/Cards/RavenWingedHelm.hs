module LordOfTheRings.Attachment.Cards.RavenWingedHelm (ravenWingedHelm) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype RavenWingedHelm = RavenWingedHelm AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero with sentinel. Limit 1 per hero, on its card def.
ravenWingedHelm :: AttachmentCard RavenWingedHelm
ravenWingedHelm = attachment RavenWingedHelm Cards.ravenWingedHelm & attachTo (AttachToHero $ HeroWithKeyword Sentinel)

-- | Response: Exhaust Raven-winged Helm to prevent 1 point of damage just dealt to attached
-- character.
instance HasAbilities RavenWingedHelm where
  getAbilities (RavenWingedHelm a) =
    [mkAbility a 1 $ ResponseAbility (dealtDamage host) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage RavenWingedHelm where
  runMessage msg s@(RavenWingedHelm a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> removeTokens a host Damage 1
      pure s
    _ -> RavenWingedHelm <$> runMessage msg a
