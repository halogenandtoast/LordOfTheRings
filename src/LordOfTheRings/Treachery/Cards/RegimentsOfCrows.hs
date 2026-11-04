module LordOfTheRings.Treachery.Cards.RegimentsOfCrows (regimentsOfCrows) where

import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageEachActiveLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RegimentsOfCrows = RegimentsOfCrows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

regimentsOfCrows :: TreacheryCard RegimentsOfCrows
regimentsOfCrows = treachery RegimentsOfCrows Cards.regimentsOfCrows

{- | When Revealed: Place 3 damage on each active location.
Shadow: Undefended damage from this attack must be assigned to the active location; of two
active locations, the one the first player chooses (FAQ 1.34).
-}
instance RunMessage RegimentsOfCrows where
  runMessage msg t@(RegimentsOfCrows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      damageEachActiveLocation attrs 3
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      lead <- getLead
      actives <- select IsActiveLocation
      chooseOrRunOneM lead do
        questionLabeled "theRingGoesSouth.activeLocation"
        causedBy attrs
        targets actives \l -> attackModifier attrs eid (UndefendedDamageTo $ toTarget l)
      pure t
    _ -> RegimentsOfCrows <$> runMessage msg attrs
