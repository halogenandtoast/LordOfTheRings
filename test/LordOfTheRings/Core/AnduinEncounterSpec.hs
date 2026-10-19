module LordOfTheRings.Core.AnduinEncounterSpec (spec) where

import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import LordOfTheRings.I18n (cardVar, ikeyWith)
import TestImport

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard >> resolveRevealed

enemiesWith :: CardDef -> GameT [EnemyId]
enemiesWith def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

theEnemy :: CardDef -> GameT EnemyId
theEnemy def = fromJustNote "no such enemy" . headMay <$> enemiesWith def

theLocation :: GameT LocationId
theLocation = getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

prompt :: PlayerId -> GameT (Maybe Text)
prompt pid = getQuestion pid <&> \case
  Just (QuestionLabel l _ _ _) -> Just l
  _ -> Nothing

spec :: Spec
spec = do
  describe "Wolf Rider" do
    it "as a shadow comes into play and attacks the defending player" . gameTest $ \pid -> do
      reveal Enemies.forestSpider []
      eid <- theEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      [shadow] <- traverse genEncounterCard [Enemies.wolfRider]
      run $ AttachShadowCard eid shadow
      _ <- setEncounterDeck [Locations.forestGate]
      run $ ResolveShadow eid pid Nothing
      riders <- enemiesWith Enemies.wolfRider
      length riders `shouldBe` 1
      prompt pid `shouldReturn` Just (ikeyWith "label.declareDefender" (cardVar "enemy" Enemies.wolfRider))

  describe "Hill Troll" do
    it "raises the player's threat by the excess damage of its attack" . gameTest $ \pid -> do
      reveal Enemies.hillTroll []
      eid <- theEnemy Enemies.hillTroll
      run $ EngageEnemy pid eid
      before <- threat pid
      run $ UndefendedDamage pid eid
      chooseTarget pid =<< hero "01007"
      -- 6 damage against Éowyn's 3 hit points
      threat pid `shouldReturn` before + 3

  describe "Goblin Sniper" do
    it "cannot be optionally engaged while other enemies are in the staging area" . gameTest $ \pid -> do
      reveal Enemies.goblinSniper []
      reveal Enemies.hummerhorns []
      run $ OptionalEngagement pid
      sniper <- theEnemy Enemies.goblinSniper
      Just question <- getQuestion pid
      [() | TargetLabel (EnemyTarget e) _ <- questionChoices question, e == sniper] `shouldBe` []

    it "has each player deal 1 damage to a character at the end of combat" . gameTest $ \pid -> do
      reveal Enemies.goblinSniper []
      run $ EndPhase CombatPhase
      eowyn <- hero "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 1

  describe "Marsh Adder" do
    it "raises the attacked player's threat by 1" . gameTest $ \pid -> do
      reveal Enemies.marshAdder []
      eid <- theEnemy Enemies.marshAdder
      before <- threat pid
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      threat pid `shouldReturn` before + 1

  describe "Wargs" do
    it "returns to the staging area after attacking with a blank shadow card" . gameTest $ \pid -> do
      reveal Enemies.wargs []
      eid <- theEnemy Enemies.wargs
      run $ EngageEnemy pid eid
      [blank] <- traverse genEncounterCard [Locations.forestGate]
      run $ AttachShadowCard eid blank
      run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
      getsGame (fmap (enemyPlacement . toAttrs) . lookup eid . mapToList . (.entities.enemies)) `shouldReturn` Just StagingArea

    it "as a shadow gives the attacker +2 attack when undefended" . gameTest $ \pid -> do
      reveal Enemies.hummerhorns []
      eid <- theEnemy Enemies.hummerhorns
      [shadow] <- traverse genEncounterCard [Enemies.wargs]
      run $ AttachShadowCard eid shadow
      run $ ResolveShadow eid pid Nothing
      getEnemyAttack eid `shouldReturn` 4

  describe "Misty Mountain Goblins" do
    it "removes 1 progress from the quest after attacking" . gameTest $ \pid -> do
      flipFirstQuest pid
      quest <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource quest Progress 3
      reveal Enemies.mistyMountainGoblins []
      eid <- theEnemy Enemies.mistyMountainGoblins
      run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
      (.progress) <$> getActiveQuest `shouldReturn` 2

  describe "Eastern Crows" do
    it "is shuffled back into the encounter deck when defeated" . gameTest $ \_ -> do
      reveal Enemies.easternCrows [Locations.forestGate]
      eid <- theEnemy Enemies.easternCrows
      run $ DealDamage GameSource (toTarget eid) 1
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Enemies.easternCrows))

  describe "Despair" do
    it "removes 4 progress from the current quest" . gameTest $ \pid -> do
      flipFirstQuest pid
      quest <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource quest Progress 5
      reveal Treacheries.despair []
      (.progress) <$> getActiveQuest `shouldReturn` 1

  describe "Massing at Night" do
    it "reveals an additional card for each player" . gameTest $ \_ -> do
      reveal Treacheries.massingAtNight [Locations.forestGate]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

  describe "Evil Storm" do
    it "damages each character of a player with 35 or more threat" . gameTest $ \pid -> do
      run $ RaiseThreat pid 35
      reveal Treacheries.evilStorm []
      traverse (getCharacterDamage <=< hero) ["01012", "01007", "01002"] `shouldReturn` [1, 1, 1]

  describe "Pursued by Shadow" do
    it "raises threat by 1 for each character not committed to the quest" . gameTest $ \pid -> do
      before <- threat pid
      reveal Treacheries.pursuedByShadow []
      threat pid `shouldReturn` before + 3

  describe "Treacherous Fog" do
    it "gives each location in the staging area +1 threat" . gameTest $ \_ -> do
      reveal Locations.forestGate []
      reveal Treacheries.treacherousFog []
      (getLocationThreat =<< theLocation) `shouldReturn` 3

  describe "The Brown Lands" do
    it "gets 1 progress after the players travel there, which explores it" . gameTest $ \_ -> do
      reveal Locations.theBrownLands []
      run . TravelTo =<< theLocation
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.theBrownLands]

  describe "The East Bight" do
    it "must be travelled to when there is no active location" . gameTest $ \pid -> do
      reveal Locations.theEastBight []
      reveal Locations.forestGate []
      run $ BeginPhase TravelPhase
      prompt pid `shouldReturn` Just "$label.mustTravel"

  describe "Banks of the Anduin" do
    it "returns to the top of the encounter deck when it leaves play" . gameTest $ \_ -> do
      reveal Locations.banksOfTheAnduin []
      lid <- theLocation
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      take 1 . map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.banksOfTheAnduin]

  describe "Gladden Fields" do
    it "raises each player's threat by an extra point in the refresh phase" . gameTest $ \pid -> do
      reveal Locations.gladdenFields []
      run . TravelTo =<< theLocation
      before <- threat pid
      run $ BeginPhase RefreshPhase
      threat pid `shouldReturn` before + 2
