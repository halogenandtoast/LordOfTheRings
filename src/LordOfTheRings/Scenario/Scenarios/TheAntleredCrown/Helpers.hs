{- | What the cards of The Antlered Crown share: the Raven deck beside the quest deck, made
of the enemies of Dunland Raiders and Dunland Warriors with a discard pile of its own, and
the time counters on its locations.
-}
module LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (
  Meta (..),
  RavenDraw (..),
  getMeta,
  isRavenCard,
  revealRavenCard,
  addRavenCardToStaging,
  putRavenCardIntoPlayEngaged,
  dealRavenShadowCardsDuringAttack,
  chooseRavenEnemy,
  shuffleRavenDiscardIntoDeck,
  assignDamage,
  removeTimeCountersFromLocations,
  activeLocationTimeX,
  removeTimeCounterFromActiveLocation,
  removeTimeCounterFromEachLocation,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, cdCardType, cdEncounterSet, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Classes.HasQueue (HasQueue, push)
import LordOfTheRings.EncounterSet (EncounterSet (DunlandRaiders, DunlandWarriors))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs)
import LordOfTheRings.Source
import LordOfTheRings.Target

-- | The scenario's own state: the Raven deck, top first, its discard pile, and the cards
-- it was made of, which go back to that discard pile as they leave play.
data Meta = Meta
  { ravenDeck :: [EncounterCard]
  , ravenDiscard :: [EncounterCard]
  , ravenCards :: [CardId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta [] [] []) $ parseMaybe parseJSON attrs.meta

-- | What becomes of the top card of the Raven deck.
data RavenDraw
  = -- | revealed as if from the encounter deck, to the staging area
    RavenReveal
  | -- | added to the staging area, not revealed
    RavenToStaging
  | -- | put into play engaged with the player
    RavenEngaged PlayerId
  | -- | dealt to the enemy as a shadow card
    RavenShadow EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An enemy of Dunland Raiders or Dunland Warriors: the Raven deck is made of them, and of
-- the nightmare Dunlending Horde, which bears the Dunland Warriors icon.
isRavenCard :: EncounterCard -> Bool
isRavenCard card =
  cdCardType def == EnemyType
    && (cdEncounterSet def `elem` map Just [DunlandRaiders, DunlandWarriors] || toCardCode card == toCardCode Enemies.dunlendingHorde)
 where
  def = toCardDef card

-- | The top card of the Raven deck, its discard pile shuffled in first when it is empty.
drawRaven :: ReverseQueue m => RavenDraw -> m ()
drawRaven = push . ScenarioSpecific "raven.draw" . toJSON

-- | "Reveal the top card of the Raven deck and add it to the staging area."
revealRavenCard :: ReverseQueue m => m ()
revealRavenCard = drawRaven RavenReveal

-- | "Take the top card of the Raven deck and add it to the staging area."
addRavenCardToStaging :: ReverseQueue m => m ()
addRavenCardToStaging = drawRaven RavenToStaging

-- | "Draw the top card of the Raven deck and put it into play engaged with him."
putRavenCardIntoPlayEngaged :: ReverseQueue m => PlayerId -> m ()
putRavenCardIntoPlayEngaged = drawRaven . RavenEngaged

{- | Deal the attacking enemy more shadow cards from the Raven deck as its shadow effects
resolve: they turn face up and resolve as any dealt during the attack do.
-}
dealRavenShadowCardsDuringAttack
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => EnemyId -> PlayerId -> [Target] -> Int -> t m ()
dealRavenShadowCardsDuringAttack eid pid defenders n = do
  replicateM_ n $ drawRaven (RavenShadow eid)
  dealShadowCardsDuringAttack eid pid defenders 0

-- | The player chooses an enemy from the Raven deck, of a title not yet in play, for the
-- staging area.
chooseRavenEnemy :: ReverseQueue m => PlayerId -> m ()
chooseRavenEnemy = push . ScenarioSpecific "raven.choose" . toJSON

shuffleRavenDiscardIntoDeck :: ReverseQueue m => m ()
shuffleRavenDiscardIntoDeck = push $ ScenarioSpecific "raven.shuffleDiscard" Null

-- | The player assigns the damage among the characters he controls, then it is dealt, as
-- "must deal X damage divided among" is (FAQ 3.117).
assignDamage :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
assignDamage = dealDamageDivided

-- | The player removes this many time counters from the locations in play, 1 at a time,
-- choosing which they come from.
removeTimeCountersFromLocations :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
removeTimeCountersFromLocations source pid n =
  when (n > 0) $ push $ ScenarioSpecific "removeTimeCounters" (toJSON (toSource source, pid, n))

-- | The Time X value on the active location, 0 when there is none.
activeLocationTimeX :: HasGame m => m Int
activeLocationTimeX = sum <$> (traverse (getTimeX . toTarget) =<< select IsActiveLocation)

-- | "Remove 1 time counter from the active location, if able."
removeTimeCounterFromActiveLocation :: (ReverseQueue m, Sourceable source) => source -> m ()
removeTimeCounterFromActiveLocation source = do
  active <- select IsActiveLocation
  for_ active \l -> do
    n <- getTimeCounters (toTarget l)
    when (n > 0) $ removeTimeCounters source l 1

-- | "Remove 1 time counter from each location in play."
removeTimeCounterFromEachLocation :: (ReverseQueue m, Sourceable source) => source -> m ()
removeTimeCounterFromEachLocation source = do
  locations <- select AnyLocation
  for_ locations \l -> do
    n <- getTimeCounters (toTarget l)
    when (n > 0) $ removeTimeCounters source l 1
