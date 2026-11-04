-- | What The Siege of Gondor's cards share: Corsair enemies, Ship locations, and the
-- resources on The Corsair Fleet.
module LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (
  corsairEnemy,
  shipLocation,
  shipIsActive,
  corsairFleetResources,
  searchForCorsairsWithACaptain,
  runSiegeOfGondorStep,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardType (EnemyType, LocationType), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Corsair, Ship))

corsairEnemy :: CardMatcher
corsairEnemy = CardWithType EnemyType <> CardWithTrait Corsair

shipLocation :: CardMatcher
shipLocation = CardWithType LocationType <> CardWithTrait Ship

-- | "If the active location is a Ship".
shipIsActive :: HasGame m => m Bool
shipIsActive = selectAny (IsActiveLocation <> LocationWithTrait Ship)

-- | The resources on The Corsair Fleet, 0 while it is not in play.
corsairFleetResources :: HasGame m => m Int
corsairFleetResources =
  getsGame
    $ sum
    . map (.resources)
    . filter ((== toCardCode Objectives.theCorsairFleet) . toCardCode)
    . toList
    . entitiesObjectives
    . gameEntities

{- | "Each player searches the encounter deck and discard pile for a Corsair enemy and adds
it to the staging area. One of those enemies must be Corsair Captain, if able": in turn
order each player chooses his, and the last must take a Corsair Captain while none has been
added by the others and one can be found.
-}
searchForCorsairsWithACaptain :: (ReverseQueue m, Targetable source) => source -> m ()
searchForCorsairsWithACaptain source = do
  players <- getRemainingPlayers
  enemies <- select AnyEnemy
  push $ ScenarioSpecific "corsairSearch" (toJSON (toTarget source, players, enemies))

-- | The scenario's own steps, each read again as it runs.
runSiegeOfGondorStep :: ReverseQueue m => Text -> Value -> m ()
runSiegeOfGondorStep key v = case key of
  "corsairSearch" | Just (source, players, before) <- parseMaybe parseJSON v ->
    for_ (uncons players) \(pid, rest :: [PlayerId]) -> do
      captains <- select (EnemyWithCardCode $ toCardCode Enemies.corsairCaptain)
      scenario <- getsGame (toAttrs . gameScenario)
      let captain = CardWithCode (toCardCode Enemies.corsairCaptain)
          captainAdded = any (`notElem` (before :: [EnemyId])) captains
          captainFound = any (cardMatches captain) (scenario.encounterDeck <> scenario.encounterDiscard)
          mustTakeCaptain = null rest && not captainAdded && captainFound
      searchEncounterDeckAndDiscard pid (source :: Target) (if mustTakeCaptain then captain else corsairEnemy)
      push $ ScenarioSpecific "corsairSearch" (toJSON (source, rest, before))
  _ -> pure ()
