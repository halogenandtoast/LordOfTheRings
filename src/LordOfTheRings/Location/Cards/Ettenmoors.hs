module LordOfTheRings.Location.Cards.Ettenmoors (ettenmoors) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)
import LordOfTheRings.Trait (Trait (Nazgul))

newtype Ettenmoors = Ettenmoors LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

ettenmoors :: LocationCard Ettenmoors
ettenmoors = location Ettenmoors Cards.ettenmoors

-- | While Ettenmoors is the active location, do not deal Nazgûl enemies a shadow card
-- during the combat phase: each is dealt none whenever it would be dealt one.
instance HasModifiersFor Ettenmoors where
  getModifiersFor (Ettenmoors a) = do
    phase <- getsGame gamePhase
    when (a.placement == ActiveLocation && phase == CombatPhase) do
      nazgul <- select (EnemyWithTrait Nazgul)
      for_ nazgul \e -> modified a e [ShadowCardsDealt 0]

-- | Forced: After any amount of progress is placed on Ettenmoors, reduce the Ring-bearer's
-- life by 1.
instance HasAbilities Ettenmoors where
  getAbilities (Ettenmoors a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

instance RunMessage Ettenmoors where
  runMessage msg l@(Ettenmoors a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      reduceRingBearersLife 1
      pure l
    _ -> Ettenmoors <$> runMessage msg a
