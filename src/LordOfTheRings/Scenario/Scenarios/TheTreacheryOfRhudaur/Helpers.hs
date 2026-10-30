-- | What the cards of The Treachery of Rhudaur share: its side quests and the Clue
-- objectives they turn into, Amarthiúl, and the Undead its cards search for.
module LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (
  undeadEnemy,
  getRhudaurSideQuests,
  flipToClue,
  controlsClue,
  cluesOnCharacters,
  clueAbilities,
  runClue,
  removeProgressPerDamage,
  currentQuestProgress,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardDef, CardType (EnemyType), cdCardTraits, cdEncounterSet, genEncounterCard, toCard, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.RunMessage (runMessage)
import LordOfTheRings.EncounterSet (EncounterSet (TheTreacheryOfRhudaur))
import LordOfTheRings.Entities (entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id (EnemyId, PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (AttackDamageRemovesProgress))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Runner ()
import LordOfTheRings.Objective.Types (ObjectiveAttrs)
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs, isEncounterSideQuest)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Clue, Undead))
import LordOfTheRings.Window

-- | An Undead enemy card, as the searches and discards read.
undeadEnemy :: CardMatcher
undeadEnemy = CardMatchAll [CardWithType EnemyType, CardWithTrait Undead]

-- | The Treachery of Rhudaur side quests in play.
getRhudaurSideQuests :: HasGame m => m [Target]
getRhudaurSideQuests =
  getsGame (map toTarget . filter rhudaur . toList . entitiesQuests . gameEntities)
 where
  rhudaur q = isEncounterSideQuest q && cdEncounterSet (toCardDef q) == Just TheTreacheryOfRhudaur

{- | "When this quest is defeated, flip it over": defeated, the side quest leaves for the
victory display, from which it comes back as its Clue objective, in the staging area.
-}
flipToClue :: ReverseQueue m => QuestAttrs -> CardDef -> m ()
flipToClue attrs clue = do
  removeFromVictoryDisplay (toCard attrs)
  addToStaging =<< genEncounterCard clue

-- | The controller of each character a Clue objective is attached to.
cluesHeld :: HasGame m => m [Maybe PlayerId]
cluesHeld = do
  objectives <- getsGame (toList . entitiesObjectives . gameEntities)
  for [host | o <- objectives, Clue `member` cdCardTraits (toCardDef o), AttachedTo host <- [(toAttrs o).placement]]
    characterController

-- | "If the defending player does not control at least 1 Clue objective."
controlsClue :: HasGame m => PlayerId -> m Bool
controlsClue pid = elem (Just pid) <$> cluesHeld

-- | The Clue objectives attached to characters, as Thaurdir's Pursuit counts them.
cluesOnCharacters :: HasGame m => m Int
cluesOnCharacters = length . filter isJust <$> cluesHeld

{- | Action: Exhaust Amarthiúl or a hero to claim the objective. Then, attach it to that
character. Any player may, while it is in the staging area.
-}
clueAbilities :: ObjectiveAttrs -> [Ability]
clueAbilities a =
  [ anyPlayer $ mkAbility a 1 $ ActionAbility $ ExhaustCharacterCost (HeroControlledBy ActivePlayer) amarthiul
  | a.placement == StagingArea
  ]
 where
  amarthiul = AllyControlledBy ActivePlayer <> AllyWithCardCode (toCardCode Objectives.amarthiulTheTreacheryOfRhudaur)

-- | The character exhausted to pay claims the objective.
runClue :: Message -> ObjectiveAttrs -> QueueT Message GameT ObjectiveAttrs
runClue msg attrs = case msg of
  UseCardAbility _ (isSource attrs -> True) 1 windows -> do
    for_ [c | Window _ (Exhausted c) <- windows] $ attachObjective attrs.id
    pure attrs
  _ -> runMessage msg attrs

-- | "Shadow: For each point of damage dealt by this attack, remove 1 progress from each
-- quest in play."
removeProgressPerDamage :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> m ()
removeProgressPerDamage source eid = attackModifier source eid AttackDamageRemovesProgress

-- | The progress on the current quest, as "while the current quest has 1 or more progress"
-- reads.
currentQuestProgress :: HasGame m => m Int
currentQuestProgress = do
  current <- getCurrentQuest
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  pure $ sum [q.progress | q <- quests, Just (toTarget q) == current]
