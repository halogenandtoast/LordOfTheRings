-- | What the cards of The Fords of Isen share: Gríma, the Dunland enemies the players
-- search for, and the hands they count.
module LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers (
  dunlandEnemy,
  handSize,
  getGrima,
  firstPlayerControlsGrima,
  searchForDifferentDunlandEnemies,
  searchToRevealDunlandEnemy,
  assignDamageAmongCharacters,
  runFordsOfIsenStep,
) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card (EncounterCard), CardCode, CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (entitiesEnemies, entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.I18n (intVar)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Dunland))

dunlandEnemy :: CardMatcher
dunlandEnemy = CardWithType EnemyType <> CardWithTrait Dunland

handSize :: HasGame m => PlayerId -> m Int
handSize pid = length . (.hand) <$> getPlayer pid

grimaCode :: CardCode
grimaCode = toCardCode Objectives.grimaTheFordsOfIsen

-- | Gríma, as an ally or still an objective on The Islet.
getGrima :: HasGame m => m (Maybe Target)
getGrima = do
  ally <- select (AllyWithCardCode grimaCode)
  objective <-
    getsGame (filter ((== grimaCode) . toCardCode) . toList . entitiesObjectives . gameEntities)
  pure $ headMay $ map toTarget ally <> map toTarget objective

firstPlayerControlsGrima :: HasGame m => m Bool
firstPlayerControlsGrima = do
  lead <- getLead
  selectAny (AllyWithCardCode grimaCode <> AllyControlledBy (PlayerWithId lead))

{- | "Each player searches the encounter deck and discard pile for 1 different Dunland
enemy and adds it to the staging area": in turn, each player's choice is barred to those
after him.
-}
searchForDifferentDunlandEnemies :: (ReverseQueue m, Targetable source) => source -> m ()
searchForDifferentDunlandEnemies source = do
  players <- getRemainingPlayers
  enemies <- select AnyEnemy
  push $ ScenarioSpecific "dunlandSearch" (toJSON (toTarget source, players, [] :: [CardCode], enemies))

{- | The player searches the encounter deck and discard pile for a Dunland enemy, reveals
it and adds it to the staging area, then the encounter deck is shuffled.
-}
searchToRevealDunlandEnemy :: (ReverseQueue m, Targetable source) => PlayerId -> source -> m ()
searchToRevealDunlandEnemy pid source = do
  scenario <- getsGame (toAttrs . gameScenario)
  chooseOneM pid do
    questionLabeled "theFordsOfIsen.searchDunland"
    causedBy source
    showing $ map EncounterCard $ scenario.encounterDeck <> scenario.encounterDiscard
    targets (filter (cardMatches dunlandEnemy) scenario.encounterDeck) \c -> do
      removeFromEncounterDeck c
      push $ RevealCard c
    targets (filter (cardMatches dunlandEnemy) scenario.encounterDiscard) revealFromEncounterDiscard
  shuffleEncounterDeck

-- | The player assigns the damage among the characters he controls, 1 at a time.
assignDamageAmongCharacters :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
assignDamageAmongCharacters source pid n =
  when (n > 0) $ push $ ScenarioSpecific "assignDamage" (toJSON (toSource source, pid, n))

-- | The scenario's own steps, each read again as it runs.
runFordsOfIsenStep :: ReverseQueue m => Text -> Value -> m ()
runFordsOfIsenStep key v = case key of
  "dunlandSearch" | Just (source, players, chosen, before) <- parseMaybe parseJSON v -> do
    now <- getsGame (entitiesEnemies . gameEntities)
    let added = [toCardCode e | e <- toList now, toId e `notElem` (before :: [EnemyId])]
        barred = chosen <> added
    for_ (uncons players) \(pid, rest) -> do
      searchEncounterDeckAndDiscard pid (source :: Target) (dunlandEnemy <> NotCard (CardMatchAny $ map CardWithCode barred))
      push $ ScenarioSpecific "dunlandSearch" (toJSON (source, rest :: [PlayerId], barred, Map.keys now))
  "assignDamage" | Just (source :: Source, pid, n) <- parseMaybe parseJSON v -> do
    characters <- charactersControlledBy pid
    chooseOneM pid do
      questionLabeledWith "theFordsOfIsen.assignDamage" [intVar "n" (n :: Int)]
      traverse_ causedBy (sourceTarget source)
      targets characters \c -> do
        dealDamage source c 1
        assignDamageAmongCharacters source pid (n - 1)
  _ -> pure ()
