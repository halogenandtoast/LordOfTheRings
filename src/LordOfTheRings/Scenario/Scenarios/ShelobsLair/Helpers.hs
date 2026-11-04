-- | What Shelob's Lair's cards share: Shelob, and what is placed on her or she does.
module LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (
  shelob,
  getShelob,
  placeResourceOnShelob,
  shelobTravelModifiers,
  cleftInVictoryDisplay,
  cardInVictoryDisplay,
  discardRandomFromEachHand,
) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Sourceable)

shelob :: EnemyMatcher
shelob = EnemyWithCardCode (toCardCode Enemies.shelob)

getShelob :: HasGame m => m (Maybe EnemyId)
getShelob = selectOne shelob

-- | "Place 1 resource on Shelob": nothing while she is out of play.
placeResourceOnShelob :: (ReverseQueue m, Sourceable source) => source -> m ()
placeResourceOnShelob source = getShelob >>= traverse_ \e -> placeResources source e 1

-- | "Travel: Place 1 resource on Shelob to travel here": not while she is out of play.
shelobTravelModifiers :: HasModifiersM m => LocationAttrs -> m ()
shelobTravelModifiers a = do
  none <- selectNone shelob
  when none $ modified a a [CannotBeTraveledTo]

cardInVictoryDisplay :: HasGame m => CardDef -> m Bool
cardInVictoryDisplay def =
  getsGame \g -> toCardCode def `elem` [toCardCode c | EncounterCard c <- (toAttrs g.scenario).victoryDisplay]

cleftInVictoryDisplay :: HasGame m => m Bool
cleftInVictoryDisplay = cardInVictoryDisplay Locations.cleftOfCirithUngol

-- | "Discard N random cards from each player's hand."
discardRandomFromEachHand :: ReverseQueue m => Int -> m ()
discardRandomFromEachHand n = eachPlayer \p -> discardRandomFromHand p n
