-- | What the cards of The Wastes of Eriador share: the Time objective, Day or Night as it
-- shows, and the Warg enemies.
module LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (
  isDay,
  isNight,
  getTimeObjective,
  flipToNight,
  wargEnemy,
  discardUntilWargs,
  searchToRevealWarg,
) where

import Data.List (scanl)
import LordOfTheRings.Card (Card (EncounterCard), CardDef, CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
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
import LordOfTheRings.Trait (Trait (Warg))

-- | The Time objective in play, whichever side is faceup.
getTimeObjective :: HasGame m => m (Maybe ObjectiveId)
getTimeObjective =
  getsGame
    $ fmap toId
    . find ((`elem` [toCardCode Objectives.daybreak, toCardCode Objectives.nightfall]) . toCardCode)
    . toList
    . entitiesObjectives
    . gameEntities

timeShows :: HasGame m => CardDef -> m Bool
timeShows side = getsGame $ any ((== toCardCode side) . toCardCode) . toList . entitiesObjectives . gameEntities

-- | It is Day while Daybreak is faceup.
isDay :: HasGame m => m Bool
isDay = timeShows Objectives.daybreak

-- | It is Night while Nightfall is faceup.
isNight :: HasGame m => m Bool
isNight = timeShows Objectives.nightfall

-- | "Flip the Time objective to Nightfall": nothing, if it is already Night.
flipToNight :: ReverseQueue m => m ()
flipToNight = whenM isDay $ getTimeObjective >>= traverse_ (push . Flip . toTarget)

wargEnemy :: CardMatcher
wargEnemy = CardWithType EnemyType <> CardWithTrait Warg

{- | "Shuffle the encounter discard pile into the encounter deck and discard cards from the
top of the encounter deck until X Warg enemies are discarded. Add each discarded enemy to
the staging area": the stage reads the shuffled deck back as 'DoStep' 1 of the message,
and calls this then.
-}
discardUntilWargs :: ReverseQueue m => Int -> m ()
discardUntilWargs x = when (x > 0) do
  deck <- getsGame ((.encounterDeck) . toAttrs . gameScenario)
  let counted = zip deck (drop 1 $ scanl (\n c -> if cardMatches wargEnemy c then n + 1 else n) (0 :: Int) deck)
      discarded = map fst $ takeWhile ((< x) . snd) counted <> take 1 (dropWhile ((< x) . snd) counted)
  -- one at a time, so a deck emptied partway is reset from the cards discarded before
  for_ discarded \c -> do
    removeFromEncounterDeck c
    if cardMatches (CardWithType EnemyType) c then addToStaging c else push $ AddToEncounterDiscard [c]

{- | The player searches the encounter deck and discard pile for a Warg enemy, reveals it
and adds it to the staging area, or puts it into play engaged with him; then the
encounter deck is shuffled.
-}
searchToRevealWarg :: (ReverseQueue m, Targetable source) => PlayerId -> source -> Bool -> m ()
searchToRevealWarg pid source engaged = do
  scenario <- getsGame (toAttrs . gameScenario)
  let reveal c = push $ if engaged then RevealCardEngaged pid c else RevealCard c
  chooseOneM pid do
    questionLabeled "theWastesOfEriador.searchWarg"
    causedBy source
    showing $ map EncounterCard $ scenario.encounterDeck <> scenario.encounterDiscard
    targets (filter (cardMatches wargEnemy) scenario.encounterDeck) \c -> removeFromEncounterDeck c >> reveal c
    targets (filter (cardMatches wargEnemy) scenario.encounterDiscard) \c -> removeFromEncounterDiscard c >> reveal c
  shuffleEncounterDeck
