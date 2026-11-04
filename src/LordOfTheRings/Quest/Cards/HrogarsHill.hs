module LordOfTheRings.Quest.Cards.HrogarsHill (hrogarsHill) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (dagnirAttacksHrogarsHill, revealSideQuest)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype HrogarsHill = HrogarsHill QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hrogarsHill :: QuestCard HrogarsHill
hrogarsHill = quest HrogarsHill Cards.hrogarsHill

{- | Forced: At the end of the planning phase, reveal the top card of the side quest deck
and add it to the staging area. Dagnir makes an attack against Hrogar's Hill. Do not deal
a shadow card for this attack. The first player may declare a defender for this attack.
Excess damage must be dealt to Hrogar's Hill.
-}
instance HasAbilities HrogarsHill where
  getAbilities (HrogarsHill a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

{- | Setup: Search the encounter deck for all 8 encounter side quests, shuffle them
together, and place them facedown next to the quest deck (the scenario did, as it built
the deck). The first player adds Dagnir to the staging area and shuffles the encounter
deck. Each other player reveals an encounter card.
This stage cannot be defeated while Dagnir has hit points remaining. If Dagnir has no hit
points remaining, the players win the game. If this stage has damage equal to its quest
points, the players lose the game.
-}
instance RunMessage HrogarsHill where
  runMessage msg q@(HrogarsHill attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      deck <- getsGame ((.scenarioEncounterDeck) . toAttrs . gameScenario)
      for_ (find ((== toCardCode Enemies.dagnir) . toCardCode) deck) \d -> do
        removeFromEncounterDeck d
        addToStaging d
      shuffleEncounterDeck
      others <- length . drop 1 <$> getRemainingPlayers
      revealEncounterCards others
      HrogarsHill <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealSideQuest
      dagnirAttacksHrogarsHill attrs
      pure q
    -- its progress piles up: Dagnir, while in play, has hit points remaining
    PlaceTokens _ (isTarget attrs -> True) Progress n -> pure $ HrogarsHill $ attrs & tokensL %~ addTokens Progress n
    CheckStage qid | qid == attrs.id -> pure q
    PlaceTokens _ (isTarget attrs -> True) Damage n -> do
      points <- getQuestPoints attrs
      when (countTokens Damage attrs.questTokens + n >= points) $ gameOver Lost
      HrogarsHill <$> runMessage msg attrs
    -- Dagnir is destroyed as she has no hit points remaining
    Destroy _ (EnemyTarget eid) -> do
      dagnir <- selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.dagnir))
      when dagnir winGame
      HrogarsHill <$> runMessage msg attrs
    _ -> HrogarsHill <$> runMessage msg attrs
