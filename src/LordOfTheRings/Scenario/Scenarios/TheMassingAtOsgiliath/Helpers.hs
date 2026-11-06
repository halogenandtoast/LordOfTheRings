module LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities ()
import LordOfTheRings.Prelude

-- Printed Introduction, “Crossing the Anduin”: players have not crossed until stage 4.
-- The river is crossed on reaching stage 4, including while a side quest is current.
crossedAnduin :: HasGame m => m Bool
crossedAnduin = getsGame $ any ((== "the-massing-at-osgiliath-019") . toCardCode) . toList . (.entities.quests)
