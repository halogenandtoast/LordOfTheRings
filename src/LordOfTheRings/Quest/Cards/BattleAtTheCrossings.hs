module LordOfTheRings.Quest.Cards.BattleAtTheCrossings (battleAtTheCrossings) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (AttackDestroyed (..), gameAttackDestroyed)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (stageSet)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype BattleAtTheCrossings = BattleAtTheCrossings QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

battleAtTheCrossings :: QuestCard BattleAtTheCrossings
battleAtTheCrossings = quest BattleAtTheCrossings Cards.battleAtTheCrossings

-- | Forced: After an enemy attacks and destroys a character, remove X progress from this
-- stage. X is the attacking enemy's printed threat.
-- This stage cannot be defeated except at the end of the combat phase: then, with enough
-- progress, it is.
instance HasAbilities BattleAtTheCrossings where
  getAbilities (BattleAtTheCrossings a) =
    [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After Nothing | a.flipped]
      <> [mkAbility a 2 $ forced $ PhaseEndsWindow When CombatPhase | a.flipped, a.progress >= a.questPoints]

{- | When Revealed: The first player takes control of Poros Garrison. Search the encounter
deck and discard pile for each enemy that belongs to an encounter set denoted by the first
icon on each set aside stage 3 and add them to the staging area. Shuffle the encounter
deck. The first icon on each stage 3 is the set it shuffles in (the cards' icons).
If the players defeat this stage, they win the game: it is the last in the quest deck.
-}
instance RunMessage BattleAtTheCrossings where
  runMessage msg q@(BattleAtTheCrossings attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      BattleAtTheCrossings <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      s <- getsGame (toAttrs . (.scenario))
      for_ (filter (cardMatches $ cardIs Objectives.porosGarrison) s.scenarioSetAside) (putObjectiveAllyIntoPlayUnder lead)
      let sets = mapMaybe (stageSet . toCardDef) s.scenarioSetAside
          hunting c = cdCardType (toCardDef c) == EnemyType && maybe False (`elem` sets) (cdEncounterSet $ toCardDef c)
      for_ (filter hunting s.encounterDeck) \c -> removeFromEncounterDeck c >> addToStaging c
      for_ (filter hunting s.encounterDiscard) \c -> removeFromEncounterDiscard c >> addToStaging c
      shuffleEncounterDeck
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> pure $ BattleAtTheCrossings $ attrs & tokensL %~ addTokens Progress n
    CheckStage qid | qid == attrs.id -> pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      -- the attacker's printed threat, read off its card as the attack destroyed the
      -- character, should it have left play since
      destroyed <- getsGame gameAttackDestroyed
      let printedThreat eid t =
            sum $ take 1 [fromMaybe 0 (cdThreat $ toCardDef c) | d <- destroyed, d.attackDestroyedBy == EnemyAttackSource eid, d.target == t, Just c <- [d.attackDestroyedAttacker]]
          xs = [printedThreat eid t | Window _ (CharacterDestroyedByAttack eid t _) <- ws]
      let n = min attrs.progress (sum xs)
      when (n > 0) $ removeTokens attrs attrs Progress n
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      completeQuest attrs.id
      pure q
    _ -> BattleAtTheCrossings <$> runMessage msg attrs
