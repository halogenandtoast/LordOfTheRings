module LordOfTheRings.Core.EscapeEncounterSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Attachment (attachMatcherFor, attachmentHosts)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

escapeTest :: (PlayerId -> GameT ()) -> IO ()
escapeTest = gameTestIn escapeFromDolGuldur starterDeck

reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

theEnemy :: CardDef -> GameT EnemyId
theEnemy def = getsGame (fromJustNote "no such enemy" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)) <&> toId

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

spec :: Spec
spec = do
  describe "Dungeon Jailor" do
    it "shuffles an unclaimed objective into the encounter deck after an unsuccessful quest" . escapeTest $ \_ -> do
      reveal Enemies.dungeonJailor []
      _ <- setEncounterDeck [Treacheries.theNecromancersReach]
      run . PutObjectiveIntoPlay =<< genEncounterCard Objectives.shadowKey
      resolveRevealed
      run QuestResolution
      getsGame (length . (.entities.objectives)) `shouldReturn` 0
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Objectives.shadowKey))

  describe "Nazgul of Dol Guldur" do
    it "cannot have attachments played on it" . escapeTest $ \pid -> do
      reveal Enemies.nazgulOfDolGuldur []
      run . EngageEnemy pid =<< theEnemy Enemies.nazgulOfDolGuldur
      attachmentHosts (attachMatcherFor $ toCardCode Attachments.forestSnare) `shouldReturn` []

    it "makes the engaged player discard a character after its shadow effect resolves" . escapeTest $ \pid -> do
      reveal Enemies.nazgulOfDolGuldur []
      eid <- theEnemy Enemies.nazgulOfDolGuldur
      run $ EngageEnemy pid eid
      [shadow] <- traverse genEncounterCard [Enemies.dolGuldurOrcs]
      run $ AttachShadowCard eid shadow
      run $ ResolveShadow eid pid Nothing
      gloin <- toTarget <$> getHeroWithCode "01003"
      chooseTarget pid gloin
      getsGame (length . (.entities.heroes)) `shouldReturn` 2

  describe "Cavern Guardian" do
    it "as an undefended shadow discards every attachment the player controls" . escapeTest $ \pid -> do
      gloin <- toTarget <$> getHeroWithCode "01003"
      _ <- putAttachmentOn pid Attachments.citadelPlate gloin
      reveal Enemies.hummerhorns []
      eid <- theEnemy Enemies.hummerhorns
      [shadow] <- traverse genEncounterCard [Enemies.cavernGuardian]
      run $ AttachShadowCard eid shadow
      run $ ResolveShadow eid pid Nothing
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "Under the Shadow" do
    it "raises the staging threat by the number of players until the end of the phase" . escapeTest $ \_ -> do
      reveal Locations.towerGate []
      reveal Treacheries.underTheShadow []
      getStagingThreat `shouldReturn` 3

  describe "Iron Shackles" do
    it "is discarded in place of the first player's next draw" . escapeTest $ \pid -> do
      reveal Treacheries.ironShackles []
      before <- handSize pid
      run $ DrawCards $ Drawing pid 1
      handSize pid `shouldReturn` before
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0
      run $ DrawCards $ Drawing pid 1
      handSize pid `shouldReturn` before + 1

  describe "Tower Gate" do
    it "sends each player an Orc Guard after travelling there" . escapeTest $ \_ -> do
      reveal Locations.towerGate []
      run . TravelTo =<< getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      getsGame (length . filter ((== "OrcGuard") . toCardCode) . toList . (.entities.enemies)) `shouldReturn` 1
