module LordOfTheRings.Treachery.Cards.TheRingsWill (theRingsWill) where

import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Trait (Trait (RingBearer))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheRingsWill = TheRingsWill TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRingsWill :: TreacheryCard TheRingsWill
theRingsWill = treachery TheRingsWill Cards.theRingsWill

instance HasModifiersFor TheRingsWill where
  getModifiersFor (TheRingsWill a) = when (a.placement == Unplaced) do
    heroes <- select (HeroWithTrait RingBearer)
    for_ heroes \h -> modified a h [CannotCancelEncounterEffects]
    whenM theOneRingExhausted $ modified a GameTarget [CannotCancelEncounterEffects]

instance RunMessage TheRingsWill where
  runMessage msg t@(TheRingsWill a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer (`encounterRaisesThreat` 5)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackNextPlayerAfterAttack eid pid
      pure t
    _ -> TheRingsWill <$> runMessage msg a
