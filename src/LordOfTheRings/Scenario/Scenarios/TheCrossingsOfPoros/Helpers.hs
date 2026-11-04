-- | What the cards of The Crossings of Poros share: the encounter sets set aside at setup,
-- which they draw from at random, and the stages the players advance to at random.
module LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (
  setAsideOf,
  withRandomFromLargestSetAside,
  discardRandomSetAsideEnemyLabeled,
  shuffleSetAsideSetIntoEncounterDeck,
  stageSet,
  runStage3A,
  playedAllies,
) where

import LordOfTheRings.Ally.Types (AllyAttrs (..))
import LordOfTheRings.Id (AllyId, PlayerId)
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PlayedCard))

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), EncounterCard, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Matcher (CardMatcher (AnyCard), getLead, getRemainingPlayers)
import LordOfTheRings.Message (Message)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck, scenarioSetAside))

-- | The set-aside cards of the encounter set.
setAsideOf :: HasGame m => EncounterSet -> m [EncounterCard]
setAsideOf set = filter ((== Just set) . cdEncounterSet . toCardDef) <$> getsGame (scenarioSetAside . toAttrs . (.scenario))

{- | Resolve the effect on a random card of the set-aside encounter set with the most cards
remaining. The scenario's own set is none of them: it holds Poros Garrison and the stages
set aside. A set removed from the game is no longer set aside (MEC61 rules). Sets tied for
the most are the first player's to choose between, as he makes the choices an encounter
card leaves to no one.
-}
withRandomFromLargestSetAside :: ReverseQueue m => (EncounterCard -> QueueT Message m ()) -> m ()
withRandomFromLargestSetAside f = do
  aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
  let sets = Map.fromListWith (<>) [(s, [c]) | c <- aside, Just s <- [cdEncounterSet (toCardDef c)], s /= TheCrossingsOfPoros]
      most = maximumMay (map length $ toList sets)
  picks <- for [(s, cs) | (s, cs) <- Map.toList sets, Just (length cs) == most] \(s, cs) -> (s,) <$> uniform cs
  lead <- getLead
  chooseOrRunOneM lead do
    questionLabeled "theCrossingsOfPoros.largestSet"
    for_ picks \(s, c) -> labeled ("theCrossingsOfPoros.set." <> tshow s) (f c)

{- | "Either place a random enemy from the set aside X set in the encounter discard pile,
or ...": offered only while the set has an enemy left set aside, as one of the two must be
done in full if it can be (FAQ 1.44).
-}
discardRandomSetAsideEnemyLabeled :: ReverseQueue m => Text -> EncounterSet -> ChooseT m ()
discardRandomSetAsideEnemyLabeled key set = do
  enemies <- lift $ filter ((== EnemyType) . cdCardType . toCardDef) <$> setAsideOf set
  unless (null enemies) do
    enemy <- lift $ uniform enemies
    labeled key $ discardSetAside [enemy]

-- | Shuffle the set-aside encounter set into the encounter deck.
shuffleSetAsideSetIntoEncounterDeck :: ReverseQueue m => EncounterSet -> m ()
shuffleSetAsideSetIntoEncounterDeck set = shuffleSetAsideIntoEncounterDeck =<< setAsideOf set

{- | The encounter set a stage 3 shuffles in, the first icon printed on it: Battle at the
Crossings adds the enemies of those of the stages set aside to the staging area.
-}
stageSet :: CardDef -> Maybe EncounterSet
stageSet def = lookup (cdCardCode def) [(cdCardCode d, s) | (d, s) <- sets]
 where
  sets = [(Quests.deadlyCreatures, DesertCreatures), (Quests.southronPursuit, HaradSoldiers), (Quests.orcPatrols, MordorOrcs)]

{- | When Revealed: Set each other stage 3A aside, out of play. Shuffle the encounter
discard pile and the stage's encounter set into the encounter deck. Then, each player
reveals an encounter card.
-}
runStage3A :: ReverseQueue m => EncounterSet -> m ()
runStage3A set = do
  others <- getsGame (filter ((== Just 3) . cdStage . toCardDef) . scenarioQuestDeck . toAttrs . (.scenario))
  for_ others \c -> do
    removeFromQuestDeck (toCardDef c)
    setAside c
  shuffleEncounterDiscardIntoDeck AnyCard
  shuffleSetAsideSetIntoEncounterDeck set
  revealEncounterCards . length =<< getRemainingPlayers

{- | The allies just played, as "after a player plays an ally" reads: each, the player who
controls it, and whether it is ready. Played from anywhere, hand or discard pile; one put
into play is not played.
-}
playedAllies :: HasGame m => [Window] -> m [(AllyId, PlayerId, Bool)]
playedAllies ws = do
  let played = [card.id | Window After (PlayedCard _ card) <- ws]
  allies <- getsGame (map toAttrs . toList . (.entities.allies))
  pure [(allyId a, pid, not (allyExhausted a)) | a <- allies, allyCardId a `elem` played, Just pid <- [placementController (allyPlacement a)]]
