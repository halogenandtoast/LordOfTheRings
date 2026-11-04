module LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany (thePassingOfTheGreyCompany) where

import Data.List (elemIndex)
import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (overcomeByFearOn)

newtype ThePassingOfTheGreyCompany = ThePassingOfTheGreyCompany ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePassingOfTheGreyCompany :: Scenario
thePassingOfTheGreyCompany =
  Scenario $ scenario ThePassingOfTheGreyCompany "the-passing-of-the-grey-company" "The Passing of the Grey Company"

{- | The Passing of the Grey Company's encounter set makes up the encounter deck (MEC54
rules). The Stone of Erech and Army of the Dead are set aside for stage 1A; Overcome by
Fear, a burden, is in no encounter set, and stage 1A attaches its copies.

The first player takes control of the Fellowship sphere Aragorn as the game sets up (see
'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage ThePassingOfTheGreyCompany where
  runMessage msg (ThePassingOfTheGreyCompany attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.ThePassingOfTheGreyCompany]
      let aside c = toCardCode c `elem` map toCardCode [Locations.theStoneOfErech, Enemies.armyOfTheDead]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.theForbiddenDoor
      laterStages <- traverse genEncounterCard [Quests.thePathsOfTheDead, Quests.theDeadAreFollowing]
      putQuestIntoPlay firstStage
      pure
        $ ThePassingOfTheGreyCompany
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    -- Campaign: Setup: Each player may change heroes without incurring the +1 threat penalty
    -- (see 'LordOfTheRings.Campaign.freeHeroChange'). The first player may raise each
    -- player's threat by 1 to search his deck for Andúril and add it to his hand. Remove
    -- each burden with the following burden set icons from the encounter deck (see
    -- 'LordOfTheRings.Saga.removedBurdenSets').
    DoStep 9 Setup | isJust attrs.campaign -> do
      mayRaiseThreatToSearchForAnduril attrs
      ThePassingOfTheGreyCompany <$> runMessage msg attrs
    -- Resolution: Add each copy of Overcome by Fear attached to a player's threat dial to
    -- the campaign pool and record the name of each player it was attached to. If Army of
    -- the Dead is in the victory display, add the Army of the Dead boon to the campaign
    -- pool. The players have earned each card added to the campaign pool.
    ResolveCampaign | Just l <- attrs.campaign -> do
      order <- getsGame (.playerOrder)
      fearful <- filterM (fmap notNull . overcomeByFearOn) order
      let fears = [PoolCard (toCardCode Objectives.overcomeByFear) (elemIndex p order) Nothing | p <- fearful]
          army = [PoolCard (toCardCode Objectives.armyOfTheDeadBoon) Nothing Nothing | toCardCode Enemies.armyOfTheDead `elem` map toCardCode attrs.victoryDisplay]
          l' = foldl' (flip addToPool) l (fears <> army)
      ThePassingOfTheGreyCompany <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> ThePassingOfTheGreyCompany <$> runMessage msg attrs
