module LordOfTheRings.Scenario.Scenarios.MountDoom (mountDoom) where

import LordOfTheRings.Campaign (CampaignLog (..), CampaignLogKey (BlackGateResources), recordedCount)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Matcher (getLead)
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Window (Timing (..), Window (..), WindowType (DiscardedForFortitudeTest, RoundEnds))

newtype MountDoom = MountDoom ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountDoom :: Scenario
mountDoom = Scenario $ scenario MountDoom "mount-doom" "Mount Doom"

-- | The Black Gate's code: The Black Gate Opens records the resources on it.
theBlackGate :: CardCode
theBlackGate = "146024"

withCardResources :: Int -> ScenarioAttrs -> ScenarioAttrs
withCardResources n attrs = attrs {scenarioCampaign = (\l -> l {campaignLogCardResources = n}) <$> attrs.campaign}

{- | Mount Doom and Deep Shadows make up the encounter deck (The Mountain of Fire rules
insert). Mount Doom, The Tower of Barad-dûr and a Brake of Thorns are set aside for stage
1A's setup.

Campaign: Setup: Each player may change heroes he controls without incurring the +1 threat
penalty (see 'LordOfTheRings.Campaign.freeHeroChange'). Place X resource tokens on this
card, where X is the number of resource tokens on The Black Gate, as recorded in the
campaign log. Remove each burden with the following burden set icons from the encounter
deck: HD, TRtI, TPotGC (see 'LordOfTheRings.Saga.removedBurdenSets'). Forced: At the end
of the round, remove 1 resource token from this card. Then, if there are no resource
tokens here, the players lose the game.

Its resolution, the first player choosing to keep or destroy The One Ring, is made as the
Ring-bearer completes his quest (see
'LordOfTheRings.Scenario.Scenarios.MountDoom.Helpers.ringBearerCompletesQuest').

Gollum: "Forced: After Gollum is discarded during a Fortitude test, put him into play
engaged with the first player. Then, choose an encounter card discarded for this test and
ignore its Eye icons." Out of play as it triggers, the scenario resolves it.
-}
instance RunMessage MountDoom where
  runMessage msg (MountDoom attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.MountDoom, EncounterSet.DeepShadows]
      let aside c = toCardCode c `elem` map toCardCode [Locations.mountDoom, Objectives.theTowerOfBaradDurMountDoom]
          brake = take 1 [c | c <- cards, toCardCode c == toCardCode Locations.brakeOfThorns]
          (setAside', deck) = partition (\c -> aside c || c `elem` brake) cards
      firstStage <- genEncounterCard Quests.theLandOfMordor
      laterStages <- traverse genEncounterCard [Quests.theRoadToIsenmouthe, Quests.theLastGasp, Quests.theCrackOfDoom]
      putQuestIntoPlay firstStage
      pure
        $ MountDoom
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    DoStep 9 Setup | Just l <- attrs.campaign -> do
      MountDoom <$> runMessage msg (withCardResources (recordedCount BlackGateResources theBlackGate l) attrs)
    CheckWindows ws -> do
      for_ [cards | Window After (DiscardedForFortitudeTest _ cards) <- ws, any isGollum cards] \cards -> do
        lead <- getLead
        for_ (find isGollum cards) (putEnemyIntoPlayEngagedFromDiscard lead)
        let others = filter (not . isGollum) cards
        chooseOrRunOneM lead do
          questionLabeled "mountDoom.ignoreEyes"
          showing (map EncounterCard others)
          for_ others \c -> targeting (CardIdTarget c.id) (ignoreFortitudeEyeIcons c.id)
      -- the campaign card's Forced effect, at the end of the round
      attrs' <- case attrs.campaign of
        Just l | Window When RoundEnds `elem` ws -> do
          let left = max 0 (l.cardResources - 1)
          when (left == 0) $ gameOver Lost
          pure $ withCardResources left attrs
        _ -> pure attrs
      MountDoom <$> runMessage msg attrs'
    ResolveCampaign | isJust attrs.campaign -> MountDoom <$> runMessage msg (withCardResources 0 attrs)
    _ -> MountDoom <$> runMessage msg attrs
   where
    isGollum c = toCardCode c == toCardCode Enemies.gollumMountDoom
