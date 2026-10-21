module LordOfTheRings.Objective.Claim (claimAbility, claimObjective) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.Types
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | Action: Raise your threat by 2 to claim this objective when it is free of encounters.
-- When claimed, attach it to a hero you control.
claimAbility :: ObjectiveAttrs -> [Ability]
claimAbility a =
  [ anyPlayer
      $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard) $ ActionAbility (RaiseThreatCost 2)
  | a.placement == StagingArea
  ]

claimObjective :: (ReverseQueue m) => PlayerId -> ObjectiveAttrs -> m ()
claimObjective pid a = do
  heroes <- select (HeroControlledBy $ PlayerWithId pid)
  chooseOneM pid do
    questionLabeled "claimObjective"
    causedBy a
    targets heroes \h -> push $ ClaimObjective a.id (toTarget h)
