module LordOfTheRings.Objective.Claim (claimAbility, claimObjective) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Objective.Types
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Target

-- | Action: Raise your threat by 2 to claim this objective when it is free of encounters.
-- When claimed, attach it to a hero you control.
claimAbility :: ObjectiveAttrs -> [Ability]
claimAbility a =
  [ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard) $ ActionAbility (RaiseThreatCost 2)
  | a.placement == StagingArea
  ]

claimObjective :: (HasGame m) => PlayerId -> ObjectiveAttrs -> m Message
claimObjective pid a = do
  heroes <- select (HeroControlledBy $ PlayerWithId pid)
  pure
    $ questionLabelWith "Attach the objective to a hero you control" (Just $ toTarget a) [] pid
      [targetLabel h [ClaimObjective a.id (toTarget h)] | h <- heroes]
