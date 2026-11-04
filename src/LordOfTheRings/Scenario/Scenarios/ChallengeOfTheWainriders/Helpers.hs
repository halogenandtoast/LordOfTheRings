{- | What the cards of Challenge of the Wainriders share: the circuit, The Challengers and
The Wainriders moving around it, the damage on The Wainriders' stage, and racing tests
(MEC80 rules).

The six stages of the circuit are kept by the scenario. Only the stage The Challengers
objective is at, the current quest, is in play as a quest card; the others wait in the
circuit, their B side faceup. A race stage defeated "remains in play and all progress is
removed from it at the end of the phase": it goes back to the circuit as The Challengers
leave it, its progress gone, the same stage it would be when The Challengers come round to
it again. The damage on The Wainriders' stage is kept with the circuit, wherever they are.

The Challengers and The Wainriders count the stages each has advanced through: The
Challengers are ahead while they have advanced through more, and either completes the
circuit twice once it has advanced through twelve.
-}
module LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (
  Circuit (..),
  RacingTest (..),
  getRace,
  raceOf,
  stageOfCircuit,
  challengersAhead,
  atWainridersStage,
  wainridersStageQuestPoints,
  placeWainridersDamage,
  removeWainridersDamage,
  racingTest,
  raceDefeated,
  RaceFailure (..),
  raceStageModifiers,
  runRaceStage,
  getWainriderChampion,
  racingResults,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardDef (cdRacingResult), EncounterCard, RacingResult (..), toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotBeDefeated, QuestPointModifier))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Progress), addTokens)

-- | The circuit, stages 1 to 6, the stages The Challengers and The Wainriders have each
-- advanced through, the damage on The Wainriders' stage, the stages the players have
-- completed this round, and the racing test being made.
data Circuit = Circuit
  { circuit :: [EncounterCard]
  , challengers :: Int
  , wainriders :: Int
  , wainridersDamage :: Int
  , completedThisRound :: Int
  , pendingTest :: Maybe RacingTest
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | A racing test: the first player commits a character by exhausting it and discards
the top X cards of the encounter deck, X its attribute; the players pass with as many of
the result as required among them, a WILD result counting as both.
-}
data RacingTest = RacingTest
  { racingSource :: Source
  , racingStat :: EscapeStat
  , racingResult :: RacingResult
  , racingRequired :: Int
  , racingOnPass :: [Message]
  , racingOnFail :: [Message]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

raceOf :: ScenarioAttrs -> Circuit
raceOf attrs = fromMaybe (Circuit [] 0 0 0 0 Nothing) $ parseMaybe parseJSON attrs.scenarioMeta

getRace :: HasGame m => m Circuit
getRace = getsGame (raceOf . toAttrs . (.scenario))

-- | The stage of the circuit a count of stages advanced through puts its objective at.
stageOfCircuit :: Circuit -> Int -> Maybe EncounterCard
stageOfCircuit r n = headMay $ drop (n `mod` 6) r.circuit

-- | "If The Challengers objective has advanced through more quest stages than The
-- Wainriders, then the players are considered to be ahead of The Wainriders."
challengersAhead :: HasGame m => m Bool
challengersAhead = (\r -> r.challengers > r.wainriders) <$> getRace

-- | The Challengers is at the same stage as The Wainriders.
atWainridersStage :: HasGame m => m Bool
atWainridersStage = (\r -> r.challengers `mod` 6 == r.wainriders `mod` 6) <$> getRace

-- | Each stage of the circuit has 0 quest points and gets +4 quest points per player.
wainridersStageQuestPoints :: HasGame m => m Int
wainridersStageQuestPoints = (4 *) . length <$> getRemainingPlayers

placeWainridersDamage :: ReverseQueue m => Int -> m ()
placeWainridersDamage n = when (n > 0) $ push $ ScenarioSpecific "wainriders.damage" (toJSON n)

-- | "Remove an equal amount of damage from The Wainriders' stage", as Narrow Pass reads.
removeWainridersDamage :: ReverseQueue m => Int -> m ()
removeWainridersDamage n = when (n > 0) $ push $ ScenarioSpecific "wainriders.damage" (toJSON (negate n))

-- | The first player makes a racing test needing as many of the result, Ravine Ledge's
-- additional results counted as it resolves; what follows a pass and a failure are actions
-- of their own.
racingTest
  :: (ReverseQueue m, Sourceable source)
  => source -> EscapeStat -> RacingResult -> Int -> QueueT Message m () -> QueueT Message m () -> m ()
racingTest source stat result required onPass onFail = do
  passed <- capture onPass
  failed <- capture onFail
  push $ ScenarioSpecific "racing.test" $ toJSON $ RacingTest (toSource source) stat result required passed failed

-- | The players defeat a race stage: The Challengers advance, the excess progress, if the
-- racing test passed, placed on the next stage.
raceDefeated :: ReverseQueue m => Maybe Int -> m ()
raceDefeated = push . ScenarioSpecific "race.defeated" . toJSON

-- | What failing a stage's racing test does: its effect, the stage still defeated, or, as
-- Home Stretch reads, "this stage is not defeated and all progress here is discarded".
data RaceFailure = StillDefeated (forall m. ReverseQueue m => QueueT Message m ()) | NotDefeated

{- | Race: the stage cannot be defeated outside of the quest phase (MEC80 rules). It gets
+4 quest points per player.
-}
raceStageModifiers :: HasModifiersM m => QuestAttrs -> m ()
raceStageModifiers a = do
  players <- length <$> getRemainingPlayers
  phase <- getsGame gamePhase
  modifySelf a $ QuestPointModifier (4 * players) : [CannotBeDefeated | phase /= QuestPhase]

{- | A stage of the circuit. Player card effects cannot place progress on it (Race). With
enough progress it is defeated: "Forced: When this stage is defeated, make a racing X test.
To pass requires 1 result, plus an additional result for each stage you have completed
this round. If the players pass the test, excess progress is placed on the next stage."
-}
runRaceStage :: EscapeStat -> RacingResult -> RaceFailure -> Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runRaceStage stat result failure msg attrs = case msg of
  PlaceTokens source (isTarget attrs -> True) Progress n
    | isPlayerCardSource source -> pure attrs
    | otherwise -> do
        let attrs' = attrs & tokensL %~ addTokens Progress n
        defeatIfDone attrs'
        pure attrs'
  CheckStage qid | qid == attrs.id -> do
    defeatIfDone attrs
    pure attrs
  _ -> runMessage msg attrs
 where
  defeatIfDone a = do
    points <- getQuestPoints a
    blocked <- elem CannotBeDefeated <$> getModifiers a
    r <- getRace
    when (points > 0 && a.progress >= points && not blocked && isNothing r.pendingTest) do
      let excess = a.progress - points
      racingTest a stat result (1 + r.completedThisRound) (raceDefeated (Just excess)) case failure of
        StillDefeated effect -> effect >> raceDefeated Nothing
        NotDefeated -> removeTokens a a Progress a.progress

-- | Wainrider Champion, in play from setup on.
getWainriderChampion :: HasGame m => m (Maybe EnemyId)
getWainriderChampion = selectOne (EnemyWithCardCode $ toCardCode Enemies.wainriderChampion)

-- | How many of the result the cards show: a WILD result counts as both a STRAIGHT and a
-- TURN result.
racingResults :: RacingResult -> [EncounterCard] -> Int
racingResults wanted = length . filter matches . mapMaybe (cdRacingResult . toCardDef)
 where
  matches r = r == wanted || r == Wild
