module LordOfTheRings.Quest.Cards.TheCoastOfUmbar (theCoastOfUmbar) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs.Helpers (rememberStormcallerDestroyed)

newtype TheCoastOfUmbar = TheCoastOfUmbar QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCoastOfUmbar :: QuestCard TheCoastOfUmbar
theCoastOfUmbar = quest TheCoastOfUmbar Cards.theCoastOfUmbar

{- | Setup: Prepare the Corsair deck. Players prepare their fleet. Add the Stormcaller to the
staging area. Add 1 copy of Southern Belfalas to the staging area (2 instead if there are 3
or 4 players in the game). Shuffle the encounter deck. The scenario has built the decks.
Sailing. Progress cannot be placed on The Coast of Umbar if the players are off-course. If
the Stormcaller is destroyed (or if this stage has 15 or more progress on it), advance to
stage 2.
-}
instance RunMessage TheCoastOfUmbar where
  runMessage msg q@(TheCoastOfUmbar attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      prepareCorsairDeck
      prepareFleet
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let copies def n = take n $ filter (cardMatches $ cardIs def) deck
      for_ (copies Enemies.stormcallerTheCityOfCorsairs 1 <> copies Locations.southernBelfalas (if players >= 3 then 2 else 1)) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      pure $ TheCoastOfUmbar $ attrs {questFlipped = True}
    PlaceTokens _ (isTarget attrs -> True) Progress _ | attrs.flipped -> do
      onCourse <- isOnCourse
      if onCourse then TheCoastOfUmbar <$> runMessage msg attrs else pure q
    CheckWindows ws | attrs.flipped, any ((== toCardCode Enemies.stormcallerTheCityOfCorsairs) . toCardCode) [c | Window After (EnemyDestroyed c) <- ws] -> do
      rememberStormcallerDestroyed
      completeQuest attrs.id
      TheCoastOfUmbar <$> runMessage msg attrs
    _ -> TheCoastOfUmbar <$> runMessage msg attrs
