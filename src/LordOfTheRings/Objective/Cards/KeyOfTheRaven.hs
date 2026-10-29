module LordOfTheRings.Objective.Cards.KeyOfTheRaven (keyOfTheRaven) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (keyAbilities, runKey)

newtype KeyOfTheRaven = KeyOfTheRaven ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

keyOfTheRaven :: ObjectiveCard KeyOfTheRaven
keyOfTheRaven = objective KeyOfTheRaven Cards.keyOfTheRaven

-- | If unattached, the first player must claim Key of the Raven and attach it to a hero he
-- controls.
instance HasAbilities KeyOfTheRaven where
  getAbilities (KeyOfTheRaven a) = keyAbilities a

-- | If Key of the Raven is discarded, add it to the staging area.
-- Shadow: Shuffle Key of the Raven back into the encounter deck.
instance RunMessage KeyOfTheRaven where
  runMessage msg (KeyOfTheRaven attrs) = KeyOfTheRaven <$> runKey msg attrs
