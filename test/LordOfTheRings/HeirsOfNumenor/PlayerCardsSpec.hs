module LordOfTheRings.HeirsOfNumenor.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Modifier (ModifierType (CannotDrawCards))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (..), attachedWithTrait, selectAny)
import LordOfTheRings.Phase
import LordOfTheRings.Trait (Trait (Gondor, Rohan))
import TestImport

-- | Boromir (leadership, Gondor), Beravor (lore) and Éowyn (spirit, Rohan).
honTest :: (PlayerId -> GameT ()) -> IO ()
honTest = gameTestWith (DeckList ["05002", "01012", "01007"] testDeck.cards)

boromirCode, beravorCode, eowynCode :: CardCode
boromirCode = "05002"
beravorCode = "01012"
eowynCode = "01007"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

heroHas :: CardCode -> Trait -> GameT Bool
heroHas code t = do
  hid <- toId <$> getHeroWithCode code
  selectAny (HeroWithId hid <> HeroWithTrait t)

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . lookup eid . mapToList . (.entities.enemies))

stage :: CardDef -> GameT EncounterCard
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  pure card

firstEnemy :: GameT EnemyId
firstEnemy = getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

allyCount :: GameT Int
allyCount = getsGame (length . (.entities.allies))

spec :: Spec
spec = describe "Heirs of Numenor player cards" do
  describe "Mutual Accord" do
    it "gives Gondor characters the Rohan trait and Rohan characters the Gondor trait" . honTest $ \pid -> do
      _ <- playWith pid boromirCode 0 Events.mutualAccord
      heroHas boromirCode Rohan `shouldReturn` True
      heroHas eowynCode Gondor `shouldReturn` True
      heroHas beravorCode Gondor `shouldReturn` False

    it "gives Gondor attachments the Rohan trait too" . honTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.stewardOfGondor beravor
      _ <- playWith pid boromirCode 0 Events.mutualAccord
      attachedWithTrait Rohan beravor >>= (`shouldSatisfy` (not . null))

  describe "Wealth of Gondor" do
    it "adds 1 resource to a Gondor hero" . honTest $ \pid -> do
      _ <- playWith pid boromirCode 0 Events.wealthOfGondor
      boromir <- getHeroWithCode boromirCode
      getHeroResources (toId boromir) `shouldReturn` 1

  describe "Spear of the Citadel" do
    it "deals 1 damage to the attacker after its character defends" . honTest $ \pid -> do
      defender <- putAllyIntoPlay pid Allies.defenderOfRammas
      spear <- putAttachmentOn pid Attachments.spearOfTheCitadel defender
      _ <- setEncounterDeck [Enemies.forestSpider]
      run RevealEncounterCard
      eid <- firstEnemy
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ BeginPhase CombatPhase
      passActions pid
      chooseTarget pid defender
      chooseAbilityOf pid spear
      enemyDamage eid `shouldReturn` 1

  describe "Damrod" do
    it "is discarded to lower threat by 1 for each enemy in the staging area" . honTest $ \pid -> do
      _ <- stage Enemies.forestSpider
      _ <- stage Enemies.forestSpider
      run $ RaiseThreat pid 20
      threat0 <- (.threat) <$> getPlayer pid
      damrod <- putAllyIntoPlay pid Allies.damrod
      run $ ActionWindow pid
      chooseAbilityOf pid damrod
      (.threat) <$> getPlayer pid `shouldReturn` threat0 - 2
      allyCount `shouldReturn` 0

  describe "Light the Beacons" do
    it "gives every character +2 defense" . honTest $ \pid -> do
      _ <- playWith pid eowynCode 5 Events.lightTheBeacons
      (getDefense =<< hero boromirCode) `shouldReturn` 4

  describe "A Watchful Peace" do
    it "returns an explored location to the top of the encounter deck" . honTest $ \pid -> do
      _ <- setEncounterDeck []
      card <- putCardIntoHand pid Events.aWatchfulPeace
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource eowyn Resource 1
      road <- stage Locations.oldForestRoad
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      chooseTarget pid (CardIdTarget card.id)
      map (.id) <$> getEncounterDeck `shouldReturn` [road.id]

    it "returns a location immune to player card effects, as it has left play" . honTest $ \pid -> do
      _ <- setEncounterDeck []
      card <- putCardIntoHand pid Events.aWatchfulPeace
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource eowyn Resource 1
      glade <- stage Locations.secludedGlade
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      chooseTarget pid (CardIdTarget card.id)
      map (.id) <$> getEncounterDeck `shouldReturn` [glade.id]

  describe "Blood of Númenor" do
    it "gives its hero +1 defense for each resource left after spending 1" . honTest $ \pid -> do
      boromir <- hero boromirCode
      blood <- putAttachmentOn pid Attachments.bloodOfNumenor boromir
      run $ PlaceTokens GameSource boromir Resource 3
      run $ ActionWindow pid
      chooseAbilityOf pid blood
      getDefense boromir `shouldReturn` 4

  describe "Hunter of Lamedon" do
    it "adds an Outlands card on top of the deck to hand" . honTest $ \pid -> do
      top <- PlayerCard <$> genPlayerCard Allies.hunterOfLamedon (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . deckL %~ (top :)
      _ <- playWith pid beravorCode 2 Allies.hunterOfLamedon
      chooseAbility pid
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [top.id]

    it "adds the card to hand while drawing is barred, as adding is not drawing" . honTest $ \pid -> do
      top <- PlayerCard <$> genPlayerCard Allies.hunterOfLamedon (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . deckL %~ (top :)
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCards] UntilEndOfRound
      _ <- playWith pid beravorCode 2 Allies.hunterOfLamedon
      chooseAbility pid
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [top.id]

  describe "Ithilien Tracker" do
    it "lowers the threat of the next enemy added to the staging area to 0" . honTest $ \pid -> do
      tracker <- putAllyIntoPlay pid Allies.ithilienTracker
      run $ ActionWindow pid
      chooseAbilityOf pid tracker
      passActions pid
      _ <- stage Enemies.forestSpider
      getStagingThreat `shouldReturn` 0
      _ <- stage Enemies.forestSpider
      getStagingThreat `shouldReturn` 2

  describe "Master of Lore" do
    it "lowers the cost of the next lore card of the named type by 1" . honTest $ \pid -> do
      master <- putAllyIntoPlay pid Allies.masterOfLore
      run $ ActionWindow pid
      chooseAbilityOf pid master
      chooseLabel "$label.masterOfLore.ally" pid
      passActions pid
      _ <- playWith pid beravorCode 1 Allies.hunterOfLamedon
      allyCount `shouldReturn` 2
