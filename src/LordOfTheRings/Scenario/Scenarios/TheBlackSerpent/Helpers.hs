-- | What the cards of The Black Serpent share: its Harad objectives.
module LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (
  haradObjectives,
  attachedToEncounterCard,
  encounterCardOf,
  haradObjectivesControlledBy,
  heroesWithObjectives,
  haradStage,
  revealRandomHaradObjective,
  randomHaradObjective,
) where

import LordOfTheRings.Card (EncounterCard, HasCardDef, IsCard (toCard), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (ObjectiveType))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified, modifySelf)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (entitiesEnemies, entitiesLocations, entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id (HeroId, PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (RevealCard))
import LordOfTheRings.Modifier (ModifierType (CannotBeDefeated, FirstPlayerClaimsWhenFree))
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Message.Lifted.Queue (ReverseQueue)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Target (Target (HeroTarget, ObjectiveTarget), Targetable (toTarget))
import LordOfTheRings.Trait (Trait (Harad))

isHarad :: HasCardDef a => a -> Bool
isHarad c = Harad `member` cdCardTraits (toCardDef c) && cdCardType (toCardDef c) == ObjectiveType

-- | The Harad objectives in play.
haradObjectives :: HasGame m => m [ObjectiveAttrs]
haradObjectives = getsGame (filter isHarad . map toAttrs . toList . entitiesObjectives . gameEntities)

-- | "While it is attached to an encounter card": an enemy or location in play guards it.
attachedToEncounterCard :: HasGame m => ObjectiveAttrs -> m Bool
attachedToEncounterCard = fmap isJust . encounterCardOf

-- | The enemy or location in play the objective is attached to, guarding it.
encounterCardOf :: HasGame m => ObjectiveAttrs -> m (Maybe Target)
encounterCardOf o = case objectiveGuard o of
  Just cid | o.placement == StagingArea -> do
    es <- getsGame gameEntities
    pure
      $ headMay
      $ [toTarget e | e <- toList (entitiesEnemies es), (toCard e).id == cid]
      <> [toTarget l | l <- toList (entitiesLocations es), (toCard l).id == cid]
  _ -> pure Nothing

heroOf :: ObjectiveAttrs -> Maybe HeroId
heroOf o = case o.placement of
  AttachedTo (HeroTarget hid) -> Just hid
  _ -> Nothing

-- | The heroes with an objective attached.
heroesWithObjectives :: HasGame m => m [HeroId]
heroesWithObjectives = getsGame (nub . mapMaybe (heroOf . toAttrs) . toList . entitiesObjectives . gameEntities)

-- | The Harad objectives the player controls: those attached to his heroes.
haradObjectivesControlledBy :: HasGame m => PlayerId -> m [ObjectiveAttrs]
haradObjectivesControlledBy pid = do
  heroes <- select (HeroControlledBy $ PlayerWithId pid)
  filter (maybe False (`elem` heroes) . heroOf) <$> haradObjectives

{- | Each stage's B side: "If a Harad objective is not attached, the first player claims it
and attaches it to a hero he controls. This stage cannot be defeated unless each Harad
objective in play is attached to a hero."
-}
haradStage :: HasModifiersM m => QuestAttrs -> m ()
haradStage a = do
  objectives <- haradObjectives
  for_ objectives \o -> modified a (ObjectiveTarget o.objectiveId) [FirstPlayerClaimsWhenFree]
  when (any (isNothing . heroOf) objectives) $ modifySelf a [CannotBeDefeated]

-- | A random set-aside Harad objective, if any is left.
randomHaradObjective :: (HasGame m, MonadRandom m) => m (Maybe EncounterCard)
randomHaradObjective = do
  aside <- getsGame (filter isHarad . scenarioSetAside . toAttrs . (.scenario))
  if null aside then pure Nothing else Just <$> uniform aside

-- | "The first player randomly selects 1 set aside Harad objective, reveals it, and adds it
-- to the staging area": its Guarded (X) finds what it is attached to.
revealRandomHaradObjective :: ReverseQueue m => m ()
revealRandomHaradObjective = traverse_ (push . RevealCard) =<< randomHaradObjective
