module LordOfTheRings.Helpers.Window (module LordOfTheRings.Helpers.Window) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Ally.Types
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Matcher
import LordOfTheRings.Prelude
import LordOfTheRings.Window

getWindowAbilities :: (HasGame m) => Window -> m [Ability]
getWindowAbilities window = do
  abilities <- concatMap getAbilities . Map.elems <$> getsGame (entitiesAllies . gameEntities)
  filterM
    ( andM
        . sequence
          [ pure . maybe False (windowMatches window) . abilityWindow
          , validCriteria . (.criteria)
          ]
    )
    abilities

validCriteria :: (HasGame m) => Criteria -> m Bool
validCriteria = \case
  LocationExists lMatcher -> selectAny lMatcher
