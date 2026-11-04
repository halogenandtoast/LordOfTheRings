module LordOfTheRings.Ally.Cards.WilyadorTheLandOfSorrow (wilyadorTheLandOfSorrow) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype WilyadorTheLandOfSorrow = WilyadorTheLandOfSorrow AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

wilyadorTheLandOfSorrow :: AllyCard WilyadorTheLandOfSorrow
wilyadorTheLandOfSorrow = ally WilyadorTheLandOfSorrow Cards.wilyadorTheLandOfSorrow

-- | Cannot have restricted attachments.
instance HasModifiersFor WilyadorTheLandOfSorrow where
  getModifiersFor (WilyadorTheLandOfSorrow a) = modifySelf a [CannotHaveRestrictedAttachments]

-- | Forced: At the end of the round, discard Wilyador unless the players as a group spend 1
-- resource, of any sphere.
instance HasAbilities WilyadorTheLandOfSorrow where
  getAbilities (WilyadorTheLandOfSorrow a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage WilyadorTheLandOfSorrow where
  runMessage msg s@(WilyadorTheLandOfSorrow a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      groupMaySpendOr pid a Neutral 1 (pure ()) (discard a)
      pure s
    _ -> WilyadorTheLandOfSorrow <$> runMessage msg a
