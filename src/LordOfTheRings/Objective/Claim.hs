module LordOfTheRings.Objective.Claim (claimAbility, claimObjective, claimableHeroes) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (cdCardTraits, cdKeywords, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id (PlayerId, HeroId)
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.Types
import LordOfTheRings.Modifier (ModifierType (CannotHaveRestrictedAttachments))
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
  heroes <- claimableHeroes pid a
  chooseOneM pid do
    questionLabeled "claimObjective"
    causedBy a
    targets heroes \h -> push $ ClaimObjective a.id (toTarget h)

-- | A claimed encounter objective obeys the host's attachment restrictions.
-- Restricted slots may be exceeded, discarding one after attachment, but a hero
-- forbidden from having Restricted cards cannot receive one in the first place.
claimableHeroes :: HasGame m => PlayerId -> ObjectiveAttrs -> m [HeroId]
claimableHeroes pid a = do
  let def = toCardDef a
  heroes <- select $ HeroControlledBy (PlayerWithId pid) <> HeroCanHaveEncounterAttachment (cdCardTraits def)
  if Restricted `member` cdKeywords def
    then filterM (fmap (notElem CannotHaveRestrictedAttachments) . getModifiers . HeroTarget) heroes
    else pure heroes
