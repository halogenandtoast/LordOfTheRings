module LordOfTheRings.Objective.Cards.KeyOfTheWolf (keyOfTheWolf) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (keyAbilities, runKey)

newtype KeyOfTheWolf = KeyOfTheWolf ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

keyOfTheWolf :: ObjectiveCard KeyOfTheWolf
keyOfTheWolf = objective KeyOfTheWolf Cards.keyOfTheWolf

-- | If unattached, the first player must claim Key of the Wolf and attach it to a hero he
-- controls.
instance HasAbilities KeyOfTheWolf where
  getAbilities (KeyOfTheWolf a) = keyAbilities a

-- | If Key of the Wolf is discarded, add it to the staging area.
-- Shadow: Shuffle Key of the Wolf back into the encounter deck.
instance RunMessage KeyOfTheWolf where
  runMessage msg (KeyOfTheWolf attrs) = KeyOfTheWolf <$> runKey msg attrs
