{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module LordOfTheRings.RingMaker.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Scenario (theDunlandTrap)
import LordOfTheRings.Scenario.Types (GameMode (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  staged def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

-- | Answer each question the player is asked with its first choice.
answerAll :: PlayerId -> GameT ()
answerAll pid =
  getQuestion pid >>= \case
    Just _ -> chooseOptionMatching "first choice" pid (const True) >> answerAll pid
    Nothing -> pure ()

heroDamage :: GameT Int
heroDamage = getsGame (sum . map (.damage) . toList . (.entities.heroes))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

enemyCount :: GameT Int
enemyCount = getsGame (length . (.entities.enemies))

spec :: Spec
spec = describe "The Ring-maker Nightmare Decks" do
  describe "The Dunland Trap" do
    it "builds the nightmare deck" . nightmare theDunlandTrap $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.boarClanWarrior, Locations.hithaeglirStream, Treacheries.lowOnProvisions, Enemies.boarClanSkirmisher]
        `shouldBe` [0, 0, 2, 4]

    it "Boar Clan Skirmisher has the engaged player damage a hero as he draws" . nightmare theDunlandTrap $ \pid -> do
      _ <- engaged pid Enemies.boarClanSkirmisher
      run $ DrawCards $ Drawing pid 1
      answerAll pid
      heroDamage `shouldReturn` 1

    it "Plains of Dunland lowers Dunland enemies' engagement cost by 20" . nightmare theDunlandTrap $ \_ -> do
      staged Locations.plainsOfDunland
      staged Enemies.boarClanAmbusher
      (getEngagementCost =<< enemyOf Enemies.boarClanAmbusher) `shouldReturn` 10

    it "Hithaeglir Woods keeps damage off Dunland enemies in the staging area" . nightmare theDunlandTrap $ \_ -> do
      staged Locations.hithaeglirWoods
      staged Enemies.boarClanAmbusher
      eid <- enemyOf Enemies.boarClanAmbusher
      getModifiers eid >>= (`shouldSatisfy` elem CannotTakeDamage)

    it "Munuv Dûv River deals a shadow card to an engaged enemy as a player draws" . nightmare theDunlandTrap $ \pid -> do
      staged Locations.munuvDuvRiver
      eid <- engaged pid Enemies.boarClanAmbusher
      _ <- setEncounterDeck [Treacheries.boarClanAssault]
      run $ DrawCards $ Drawing pid 1
      answerAll pid
      length . (.shadowCards) <$> getEnemy eid `shouldReturn` 1

    it "Boar Clan Assault surges with no engaged enemy" . nightmare theDunlandTrap $ \_ -> do
      reveal Treacheries.boarClanAssault [Locations.plainsOfDunland]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Stalked by Dunlendings adds the first player's hand size to the staging threat" . nightmare theDunlandTrap $ \pid -> do
      reveal Treacheries.stalkedByDunlendings []
      n <- handSize pid
      getStagingThreat `shouldReturn` n

    it "Stalked by Dunlendings' shadow engages the topmost enemy in the discard pile" . nightmare theDunlandTrap $ \pid -> do
      eid <- engaged pid Enemies.boarClanAmbusher
      run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.boarClanSkirmisher
      _ <- setEncounterDeck [Locations.plainsOfDunland]
      shadow pid eid Treacheries.stalkedByDunlendings []
      skirmisher <- enemyOf Enemies.boarClanSkirmisher
      length . (.shadowCards) <$> getEnemy skirmisher `shouldReturn` 1
