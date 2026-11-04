module LordOfTheRings.Scenario.Scenarios.ShelobsLair (shelobsLair) where

import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..), updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype ShelobsLair = ShelobsLair ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shelobsLair :: Scenario
shelobsLair = Scenario $ scenario ShelobsLair "shelobs-lair" "Shelob's Lair"

{- | Shelob's Lair, The Great Spider, Gollum and Morgul Nazgûl make up the encounter deck
(The Land of Shadow rules insert). Gollum is not gathered, having no encounter card back.
Stage 1A sets Cleft of Cirith Ungol, Shelob and The Great Spider aside, makes Morgul Vale
the active location and adds Sméagol's Secret Way to the staging area: all five are set
aside as the deck is gathered, for it to take them from there.

Campaign: Setup: Attach A Heavy Burden to the Ring-bearer. (The burdens of Helm's Deep and
The Road to Isengard are left out as the deck is dealt: see
'LordOfTheRings.Saga.removedBurdenSets'.) A copy the Campaign Pool already holds is
attached as the game is set up, and no other is.
Resolution: Add A Heavy Burden to the campaign pool. The players have earned that burden.
-}
instance RunMessage ShelobsLair where
  runMessage msg (ShelobsLair attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.ShelobsLair, EncounterSet.TheGreatSpider, EncounterSet.GollumSaga, EncounterSet.NazgulSaga]
      let named = map toCardCode [Locations.cleftOfCirithUngol, Enemies.shelob, Locations.morgulValeShelobsLair, Locations.smeagolsSecretWay]
          aside c = toCardCode c `elem` named || cdEncounterSet (toCardDef c) == Just EncounterSet.TheGreatSpider
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.theStairsOfCirithUngol
      laterStages <- traverse genEncounterCard [Quests.theSpidersPass, Quests.gollumsBetrayal]
      putQuestIntoPlay firstStage
      pure $ ShelobsLair $ attrs & encounterDeckL .~ deck & setAsideL .~ setAside' & questDeckL .~ laterStages
    DoStep 9 Setup | isJust attrs.campaign -> do
      burdens <- getsGame (filter ((== toCardCode Objectives.aHeavyBurden) . toCardCode) . toList . (.entities.objectives))
      when (null burdens) do
        ringBearers <- select ringBearer
        for_ (take 1 ringBearers) \hid ->
          (`putObjectiveIntoPlayAttached` HeroTarget hid) =<< genEncounterCard Objectives.aHeavyBurden
      ShelobsLair <$> runMessage msg attrs
    ResolveCampaign | Just l <- attrs.campaign -> do
      let earned = PoolCard (toCardCode Objectives.aHeavyBurden) Nothing Nothing
          l' = if earned `elem` l.pool then l else updateLog (AddToPool earned) l
      ShelobsLair <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> ShelobsLair <$> runMessage msg attrs
