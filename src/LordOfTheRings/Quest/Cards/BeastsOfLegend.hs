module LordOfTheRings.Quest.Cards.BeastsOfLegend (beastsOfLegend) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gamePhase, gameScenario)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (increaseTemperature, temperatureX)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype BeastsOfLegend = BeastsOfLegend QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

beastsOfLegend :: QuestCard BeastsOfLegend
beastsOfLegend = quest BeastsOfLegend Cards.beastsOfLegend

{- | During the quest phase, add X threat to the total threat in the staging area, where X
is the tens digit of the temperature. While at least 1 Were-worm is in play, the players
cannot defeat this stage. If the players defeat this stage, they win the game: it is the
last stage.
-}
instance HasModifiersFor BeastsOfLegend where
  getModifiersFor (BeastsOfLegend a) = when a.flipped do
    phase <- getsGame gamePhase
    x <- temperatureX
    when (phase == QuestPhase && x > 0) $ modified a GameTarget [StagingThreatBonus x]
    worms <- selectAny (EnemyWithCardCode $ toCardCode Enemies.wereWorm)
    when worms $ modifySelf a [CannotBeDefeated]

-- | Forced: At the end of the round, increase the temperature by 4. If the temperature is
-- 60 or higher, the players lose the game (the scenario checks it as it moves).
instance HasAbilities BeastsOfLegend where
  getAbilities (BeastsOfLegend a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | When Revealed: Increase the temperature by 2. Each player searches the encounter deck,
discard pile, and victory display for a Were-worm and adds it to the staging area, if
able. Shuffle the encounter deck. The copies are alike, so one is taken for each player
without asking, from the deck first.
-}
instance RunMessage BeastsOfLegend where
  runMessage msg q@(BeastsOfLegend attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      BeastsOfLegend <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      increaseTemperature 2
      players <- length <$> getRemainingPlayers
      s <- getsGame (toAttrs . gameScenario)
      let isWorm = cardMatches (cardIs Enemies.wereWorm)
          found =
            take players
              $ [(removeFromEncounterDeck c, c) | c <- s.scenarioEncounterDeck, isWorm c]
              <> [(removeFromEncounterDiscard c, c) | c <- s.scenarioEncounterDiscard, isWorm c]
              <> [(removeFromVictoryDisplay (EncounterCard c), c) | EncounterCard c <- s.scenarioVictoryDisplay, isWorm c]
      for_ found \(remove, c) -> remove >> addToStaging c
      shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      increaseTemperature 4
      pure q
    _ -> BeastsOfLegend <$> runMessage msg attrs
