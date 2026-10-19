module LordOfTheRings.Core.ObjectivesSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | An objective put into play, guarded by whatever the encounter deck gives it.
objectiveGuardedBy :: CardDef -> [CardDef] -> GameT Target
objectiveGuardedBy def guards = do
  _ <- setEncounterDeck guards
  run . PutObjectiveIntoPlay =<< genEncounterCard def
  resolveRevealed
  getsGame (toTarget . fromJustNote "no objective" . headMay . toList . (.entities.objectives))

placement :: Target -> GameT Placement
placement (ObjectiveTarget oid) = getsGame (maybe Unplaced (objectivePlacement . toAttrs) . lookup oid . mapToList . (.entities.objectives))
placement _ = pure Unplaced

claim :: PlayerId -> Target -> CardCode -> GameT ()
claim pid objective code = do
  run $ BeginPhase PlanningPhase
  chooseAbilityOf pid objective
  chooseTarget pid =<< hero code

spec :: Spec
spec = do
  describe "Objectives" do
    it "go to the staging area guarded by the next enemy, and cannot be claimed while it is in play" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.gandalfsMap [Enemies.forestSpider]
      placement objective `shouldReturn` StagingArea
      getsGame (length . (.entities.enemies)) `shouldReturn` 1
      run $ BeginPhase PlanningPhase
      offersAbilityOf pid objective `shouldReturn` False

    it "are claimed for 2 threat once free of encounters, onto a hero you control" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.gandalfsMap [Enemies.forestSpider]
      spider <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ Discard (toTarget spider)
      threat <- (.threat) <$> getPlayer pid
      eowyn <- hero "01007"
      claim pid objective "01007"
      placement objective `shouldReturn` AttachedTo eowyn
      (.threat) <$> getPlayer pid `shouldReturn` threat + 2

    it "are free at once when a treachery is dealt to guard them" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.shadowKey [Treacheries.theNecromancersReach]
      run $ BeginPhase PlanningPhase
      offersAbilityOf pid objective `shouldReturn` True

    it "return to the staging area when their hero leaves play" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.shadowKey [Treacheries.theNecromancersReach]
      claim pid objective "01007"
      passActions pid
      run . Discard =<< hero "01007"
      placement objective `shouldReturn` StagingArea

  describe "Gandalf's Map" do
    it "stops the attached hero defending" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.gandalfsMap [Treacheries.theNecromancersReach]
      claim pid objective "01007"
      passActions pid
      eowyn <- hero "01007"
      _ <- setEncounterDeck [Enemies.forestSpider]
      run RevealEncounterCard
      spider <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ EngageEnemy pid spider
      run $ EnemyAttack pid spider
      Just question <- getQuestion pid
      [() | TargetLabel t _ <- questionChoices question, t == eowyn] `shouldBe` []

  describe "Dungeon Torch" do
    it "raises its hero's controller's threat by 2 at the end of the round" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.dungeonTorch [Treacheries.theNecromancersReach]
      claim pid objective "01007"
      passActions pid
      threat <- (.threat) <$> getPlayer pid
      run EndRound
      (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  describe "Shadow Key" do
    it "deals its hero 1 damage at the end of the round" . gameTest $ \pid -> do
      objective <- objectiveGuardedBy Objectives.shadowKey [Treacheries.theNecromancersReach]
      claim pid objective "01007"
      passActions pid
      run EndRound
      (getCharacterDamage =<< hero "01007") `shouldReturn` 1
