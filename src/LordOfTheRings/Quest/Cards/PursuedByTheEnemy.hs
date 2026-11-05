module LordOfTheRings.Quest.Cards.PursuedByTheEnemy (pursuedByTheEnemy) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife, setRingBearersLife)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype PursuedByTheEnemy = PursuedByTheEnemy QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pursuedByTheEnemy :: QuestCard PursuedByTheEnemy
pursuedByTheEnemy = quest PursuedByTheEnemy Cards.pursuedByTheEnemy

-- | Forced: At the end of the round, reduce the Ring-bearer's life by 1.
instance HasAbilities PursuedByTheEnemy where
  getAbilities (PursuedByTheEnemy a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Shuffle each burden card from the Flight to the Ford burden set into a Burden
deck. Attach An Evil Wound to the Ring-bearer. Set Ford of Bruinen and The Witch-king
aside, out of play. Add The Last Bridge and 1 Fell Rider per player to the staging area.
Set the Ring-bearer's life at 15. Shuffle the encounter deck. (The scenario set Ford of
Bruinen and The Witch-king aside as it gathered the encounter deck.)

The players cannot advance unless The Last Bridge is in the victory display.
-}
instance RunMessage PursuedByTheEnemy where
  runMessage msg q@(PursuedByTheEnemy attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      createBurdenDeck =<< traverse genEncounterCard burdens
      wound <- genEncounterCard Objectives.anEvilWound
      bearers <- select ringBearer
      for_ (headMay bearers) $ putObjectiveIntoPlayAttached wound
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let bridge = take 1 $ filter (cardMatches $ cardIs Locations.theLastBridge) deck
          riders = take players $ filter (cardMatches $ CardMatchAny $ map cardIs [Enemies.fellRider, Enemies.fellRiderNightmare]) deck
      for_ (bridge <> riders) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      setRingBearersLife 15
      shuffleEncounterDeck
      PursuedByTheEnemy <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      reduceRingBearersLife 1
      pure q
    _ -> PursuedByTheEnemy <$> runConditionalStage lastBridgeInVictoryDisplay msg attrs
   where
    burdens = [Treacheries.weightOfTheRing, Treacheries.fearOfDiscovery, Treacheries.eatenAlive, Treacheries.panickedFlightToTheFord, Treacheries.overcomeByTerror]
    lastBridgeInVictoryDisplay = getsGame (any (cardMatches $ cardIs Locations.theLastBridge) . (.victoryDisplay) . toAttrs . (.scenario))
