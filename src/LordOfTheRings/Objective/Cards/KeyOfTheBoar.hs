module LordOfTheRings.Objective.Cards.KeyOfTheBoar (keyOfTheBoar) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (keyAbilities, runKey)

newtype KeyOfTheBoar = KeyOfTheBoar ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

keyOfTheBoar :: ObjectiveCard KeyOfTheBoar
keyOfTheBoar = objective KeyOfTheBoar Cards.keyOfTheBoar

-- | If unattached, the first player must claim Key of the Boar and attach it to a hero he
-- controls.
instance HasAbilities KeyOfTheBoar where
  getAbilities (KeyOfTheBoar a) = keyAbilities a

-- | If Key of the Boar is discarded, add it to the staging area.
-- Shadow: Shuffle Key of the Boar back into the encounter deck.
instance RunMessage KeyOfTheBoar where
  runMessage msg (KeyOfTheBoar attrs) = KeyOfTheBoar <$> runKey msg attrs
