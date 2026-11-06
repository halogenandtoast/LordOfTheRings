module LordOfTheRings.Helpers.QuestCommit (pendingCommitCost, commitmentSacrifices, canPayCommitCost, canCommitExistingCharacter, characterInPlay) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Id

pendingCommitCost :: HasGame m => PlayerId -> m (Maybe (QuestId, CardMatcher))
pendingCommitCost pid = do
  mods <- getModifiers (PlayerTarget pid)
  pure $ headMay [(qid, matcher) | CommitRequiresDiscard qid matcher <- mods, PaidCommitmentCost qid `notElem` mods]

commitmentSacrifices :: HasGame m => PlayerId -> QuestId -> CardMatcher -> Target -> m [Target]
commitmentSacrifices pid qid matcher committer = do
  heroes <- map HeroTarget <$> select (HeroControlledBy $ PlayerWithId pid)
  allies <- select (AllyControlledBy $ PlayerWithId pid)
  matching <- filterM (fmap (cardMatches matcher) . getAlly) allies
  filterM discardable $ filter (/= committer) (heroes <> map AllyTarget matching)
 where
  discardable t = do
    mods <- getModifiers t
    immune <- immuneToSource (QuestSource qid) t
    pure $ not immune && CannotLeavePlay `notElem` mods && CannotBeDiscardedByEncounterCards `notElem` mods

canPayCommitCost :: HasGame m => PlayerId -> Target -> m Bool
canPayCommitCost pid target = pendingCommitCost pid >>= \case
  Nothing -> pure True
  Just (qid, matcher) -> notNull <$> commitmentSacrifices pid qid matcher target

-- Recheck prohibitions after paying the cost or resolving a When window.
-- Readiness is checked by the declaring action, not here: effects such as
-- Hobbit Pony and Late Adventurer exhaust their character before this message.
canCommitExistingCharacter :: HasGame m => PlayerId -> Target -> m Bool
canCommitExistingCharacter pid target = do
  controlled <- selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
  committed <- selectCharacters CommittedHero CommittedAlly
  mods <- getModifiers target
  pure $ target `elem` controlled && target `notElem` committed && CannotCommitToQuest `notElem` mods

characterInPlay :: HasGame m => Target -> m Bool
characterInPlay target = getsGame $ \g -> case target of
  HeroTarget hid -> Map.member hid g.entities.heroes
  AllyTarget aid -> Map.member aid g.entities.allies
  _ -> False
