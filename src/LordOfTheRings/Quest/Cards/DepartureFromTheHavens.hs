module LordOfTheRings.Quest.Cards.DepartureFromTheHavens (departureFromTheHavens) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DepartureFromTheHavens = DepartureFromTheHavens QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

departureFromTheHavens :: QuestCard DepartureFromTheHavens
departureFromTheHavens = quest DepartureFromTheHavens Cards.departureFromTheHavens

-- | Sailing. While you are on-course, each Ship objective gets +2 willpower.
instance HasModifiersFor DepartureFromTheHavens where
  getModifiersFor (DepartureFromTheHavens a) = when a.flipped do
    whenM isOnCourse do
      ships <- select shipObjective
      for_ ships \s -> modified a s [WillpowerModifier 2]

-- | Setup: Prepare the Corsair deck. Players prepare their fleet. Search the encounter
-- deck for 1 copy of Rolling Seas and 1 copy of Scouting Ship, and add them to the
-- staging area. Shuffle the encounter deck. The scenario has shuffled the stage 2 cards.
-- On-course, the stage it advances to is the players' to choose: see the scenario.
instance RunMessage DepartureFromTheHavens where
  runMessage msg q@(DepartureFromTheHavens attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      prepareCorsairDeck
      prepareFleet
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ [Locations.rollingSeas, Enemies.scoutingShip] \def ->
        for_ (take 1 $ filter (cardMatches $ cardIs def) deck) \card -> do
          removeFromEncounterDeck card
          addToStaging card
      shuffleEncounterDeck
      pure $ DepartureFromTheHavens $ attrs {questFlipped = True}
    _ -> DepartureFromTheHavens <$> runMessage msg attrs
