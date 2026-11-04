module LordOfTheRings.Location.Cards.ExpanseOfHarad (expanseOfHarad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (atOrcsStage)

newtype ExpanseOfHarad = ExpanseOfHarad LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

expanseOfHarad :: LocationCard ExpanseOfHarad
expanseOfHarad = location ExpanseOfHarad Cards.expanseOfHarad

-- | While the players are not at the same stage as the Orcs' stage, Expanse of Harad gets
-- +2 threat.
instance HasModifiersFor ExpanseOfHarad where
  getModifiersFor (ExpanseOfHarad a) = unlessM atOrcsStage $ modifySelf a [ThreatModifier 2]

-- | Forced: After Expanse of Harad becomes the active location, raise each player's threat
-- by 2.
instance HasAbilities ExpanseOfHarad where
  getAbilities (ExpanseOfHarad a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage ExpanseOfHarad where
  runMessage msg l@(ExpanseOfHarad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      pure l
    _ -> ExpanseOfHarad <$> runMessage msg a
