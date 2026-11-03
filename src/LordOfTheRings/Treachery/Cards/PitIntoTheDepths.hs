module LordOfTheRings.Treachery.Cards.PitIntoTheDepths (pitIntoTheDepths) where

import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Trait (Trait (Grotto))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PitIntoTheDepths = PitIntoTheDepths TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pitIntoTheDepths :: TreacheryCard PitIntoTheDepths
pitIntoTheDepths = treachery PitIntoTheDepths Cards.pitIntoTheDepths

-- | When Revealed: Until the end of the quest phase, each ally gets -1 willpower. If the
-- active location has the Grotto trait, remove all progress from it and flip it to its
-- Underwater side (turning over removes its progress).
-- Shadow: If the active location is Underwater, treat this attack as if it were undefended.
instance RunMessage PitIntoTheDepths where
  runMessage msg t@(PitIntoTheDepths attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- select AnyAlly
      for_ allies \x -> phaseModifier attrs x (WillpowerModifier (-1))
      grotto <- select (IsActiveLocation <> LocationWithTrait Grotto)
      for_ grotto flipOver
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM underwaterActive $ makeUndefended pid eid
      pure t
    _ -> PitIntoTheDepths <$> runMessage msg attrs
