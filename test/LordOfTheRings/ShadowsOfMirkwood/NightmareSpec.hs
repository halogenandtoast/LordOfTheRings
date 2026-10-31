module LordOfTheRings.ShadowsOfMirkwood.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase
import LordOfTheRings.Scenario (theHuntForGollum)
import LordOfTheRings.Scenario.Types (GameMode (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

theEnemy :: GameT EnemyId
theEnemy = getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

enemyCardId :: EnemyId -> GameT CardId
enemyCardId eid = getsGame (fromJustNote "no enemy" . fmap (.cardId) . lookup eid . map (\e -> (toId e, toAttrs e)) . toList . (.entities.enemies))

objectives :: GameT [ObjectiveAttrs]
objectives = getsGame (map toAttrs . toList . (.entities.objectives))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

spec :: Spec
spec = describe "Shadows of Mirkwood nightmare mode" do
  describe "The Hunt for Gollum" do
    it "builds the nightmare deck" . nightmare theHuntForGollum $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 47
      map (`copies` deck) [Enemies.mistyMountainGoblins, Locations.gladdenFields, Enemies.ravenousHawk, Treacheries.restlessHunters]
        `shouldBe` [1, 0, 3, 3]

    it "attaches a free Clue card to a Mordor enemy at the end of the refresh phase" . nightmare theHuntForGollum $ \_ -> do
      reveal Enemies.huntingWarg []
      warg <- enemyCardId =<< theEnemy
      run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.signsOfGollum
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      map objectiveGuard <$> objectives `shouldReturn` [Just warg]

    it "loses the game with four Clue cards on Mordor enemies" . nightmare theHuntForGollum $ \_ -> do
      reveal Enemies.huntingWarg []
      warg <- enemyCardId =<< theEnemy
      replicateM_ 4 $ run . (`PutObjectiveIntoPlayGuardedBy` warg) =<< genEncounterCard Objectives.signsOfGollum
      getsGame (.outcome) `shouldReturn` Just Lost

    it "The Enemy's Trail attaches a Clue card from the discard pile to a Mordor enemy" . nightmare theHuntForGollum $ \_ -> do
      reveal Enemies.huntingWarg []
      warg <- enemyCardId =<< theEnemy
      run . AddToEncounterDiscard . pure =<< genEncounterCard Objectives.signsOfGollum
      reveal Treacheries.theEnemysTrail []
      map objectiveGuard <$> objectives `shouldReturn` [Just warg]

    it "The Trail Goes Cold makes the active location count in the quest phase" . nightmare theHuntForGollum $ \_ -> do
      reveal Locations.darkPools []
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ TravelTo lid
      reveal Treacheries.theTrailGoesCold []
      run $ BeginPhase QuestPhase
      getStagingThreat `shouldReturn` 3

    it "Ravenous Hawk engages the player who looks at it" . nightmare theHuntForGollum $ \pid -> do
      _ <- setEncounterDeck [Enemies.ravenousHawk]
      run $ LookAtTopOfEncounterDeck pid GameSource False
      eid <- theEnemy
      getsGame (fmap (.placement) . lookup eid . map (\e -> (toId e, toAttrs e)) . toList . (.entities.enemies))
        `shouldReturn` Just (EngagedWith pid)
