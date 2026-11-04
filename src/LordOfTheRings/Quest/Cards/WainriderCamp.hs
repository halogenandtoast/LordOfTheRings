module LordOfTheRings.Quest.Cards.WainriderCamp (wainriderCamp) where

import LordOfTheRings.Card (CardType (LocationType), RacingResult (Straight), cdName, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers

newtype WainriderCamp = WainriderCamp QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wainriderCamp :: QuestCard WainriderCamp
wainriderCamp = quest WainriderCamp Cards.wainriderCamp

-- | Race. This stage gets +4 quest points per player.
instance HasModifiersFor WainriderCamp where
  getModifiersFor (WainriderCamp a) = raceStageModifiers a

{- | Setup: Build the circuit (see insert). Place The Wainriders and The Challengers
objective cards at this stage. The first player adds Wainrider Champion to the staging
area. Each other player adds 1 different location to the staging area. Shuffle the
encounter deck. (The scenario builds the circuit as it gathers.)

Forced: When this stage is defeated, make a racing Defense test. To pass requires 1
STRAIGHT result, plus an additional STRAIGHT result for each stage you have completed this
round. If the players pass the test, excess progress is placed on the next stage. If the
players fail the test, each player discards 1 random card from his hand.
-}
instance RunMessage WainriderCamp where
  runMessage msg q@(WainriderCamp attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      objectives <- getSetAside (CardMatchAny [cardIs Objectives.theWainriders, cardIs Objectives.theChallengers])
      for_ objectives (push . PutObjectiveIntoPlay)
      push $ ScenarioSpecific "circuit.placeObjectives" Null
      addSetAsideToStaging (cardIs Enemies.wainriderChampion)
      doStep 1 msg
      WainriderCamp <$> runMessage msg attrs
    -- each other player in turn, a location whose title none in the staging area has
    DoStep n (PerformSetup (isTarget attrs -> True)) -> do
      others <- drop 1 <$> getRemainingPlayers
      case drop (n - 1) others of
        p : _ -> do
          staged <- select LocationInStagingArea
          staged' <- for staged \l -> (\(Name t _) -> t) . cdName . toCardDef <$> getLocation l
          searchEncounterDeckToStaging p (CardWithType LocationType <> NotCard (CardMatchAny $ map CardWithTitle staged'))
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure q
    _ -> WainriderCamp <$> runRaceStage EscapeDefense Straight failure msg attrs
   where
    failure = StillDefeated $ eachPlayer (`discardRandomFromHand` 1)
