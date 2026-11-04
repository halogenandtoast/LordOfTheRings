module LordOfTheRings.LordOfTheRingsSaga.PlayerCardsASpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (..), PoolCard (..), newCampaignLog)
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (EnemyAttrs (..))
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getStagingThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Keyword (Keyword (FirstPlayerGainsControl))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getThreat, hasKeyword)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

frodoCode, eowynCode, theodredCode, gandalfCode :: CardCode
frodoCode = "141001"
eowynCode = "01007"
theodredCode = "01002"
gandalfCode = "142002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Frodo Baggins of The Black Riders, with Éowyn and Théodred: 17 starting threat.
frodoDeck :: DeckList
frodoDeck = deckList [frodoCode, eowynCode, theodredCode] testDeck.cards

roadDarkensFrodoDeck :: DeckList
roadDarkensFrodoDeck = deckList ["142001", eowynCode, theodredCode] testDeck.cards

-- | The Road Darkens' Gandalf, with Éowyn and Théodred.
gandalfDeck :: DeckList
gandalfDeck = deckList [gandalfCode, eowynCode, theodredCode] testDeck.cards

-- | The scenario's One Ring on the hero, as the saga's setup attaches it.
attachTheOneRing :: CardDef -> Target -> GameT ()
attachTheOneRing ring host = run . (`PutObjectiveIntoPlayAttached` host) =<< genEncounterCard ring

theOneRing :: GameT ObjectiveAttrs
theOneRing = getsGame (fromJustNote "no One Ring" . headMay . map toAttrs . toList . (.entities.objectives))

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies)) <&> enemyId

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . Map.lookup hid <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.allies)
  AttachmentTarget aid -> maybe False (attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (.entities.attachments)
  _ -> pure False

putOnTopOfDeck :: PlayerId -> CardDef -> GameT Card
putOnTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . deckL %~ (card :)) =<< getGame
  pure card

-- | The scenario under another id, as the saga's setup reads it.
asScenario :: ScenarioId -> Scenario
asScenario sid = overAttrs (\a -> a {scenarioId = sid}) passageThroughMirkwood

spec :: Spec
spec = describe "The Black Riders, The Road Darkens and The Treason of Saruman player cards" do
  describe "the saga's setup" do
    it "gives the first player the Black Riders' Frodo Baggins with The One Ring" . gameTestIn (asScenario "a-knife-in-the-dark") testDeck $ \pid -> do
      run $ DoStep 3 Setup
      chooseRingBearer pid frodoCode
      frodo <- getHeroWithCode frodoCode
      frodo.owner `shouldBe` pid
      ring <- theOneRing
      toCardCode ring `shouldBe` "141085"
      objectivePlacement ring `shouldBe` AttachedTo (toTarget frodo)

    it "gives the first player The Treason of Saruman's Aragorn, following the first player token" . gameTestIn (asScenario "the-uruk-hai") testDeck $ \pid -> do
      run $ DoStep 3 Setup
      aragorn <- getHeroWithCode "143001"
      aragorn.owner `shouldBe` pid
      hasKeyword FirstPlayerGainsControl (toTarget aragorn) (toCardDef aragorn) `shouldReturn` True

  describe "The One Ring" do
    it "has the first player gain control of its Ring-bearer" . gameTestWith frodoDeck $ \_ -> do
      frodo <- hero frodoCode
      attachTheOneRing Objectives.theOneRingTheBlackRiders frodo
      hasKeyword FirstPlayerGainsControl frodo Heroes.frodoBagginsTheBlackRiders `shouldReturn` True

    it "loses the game as it leaves play with the Ring-bearer" . gameTestWith frodoDeck $ \_ -> do
      frodo <- hero frodoCode
      attachTheOneRing Objectives.theOneRingTheRoadDarkens frodo
      run $ Destroy GameSource frodo
      getsGame (.outcome) `shouldReturn` Just Lost

  describe "Aragorn" do
    it "loses the game as he leaves play" . gameTestWith (deckList ["143001", eowynCode, theodredCode] testDeck.cards) $ \_ -> do
      run . RemoveFromGame =<< hero "143001"
      getsGame (.outcome) `shouldReturn` Just Lost

    it "spends 2 of his resources to ready a hero" . gameTestWith (deckList ["143001", eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      aragorn <- hero "143001"
      eowyn <- hero eowynCode
      run $ PlaceTokens GameSource aragorn Resource 2
      run $ Exhaust eowyn
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid aragorn
      chooseTarget pid eowyn
      isExhausted eowyn `shouldReturn` False
      (getHeroResources . toId =<< getHeroWithCode "143001") `shouldReturn` 0

  describe "Frodo Baggins (The Black Riders)" do
    it "spends a Fellowship resource and exhausts The One Ring to shuffle a revealed card back and reveal another" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      attachTheOneRing Objectives.theOneRingTheBlackRiders frodo
      run $ PlaceTokens GameSource frodo Resource 1
      _ <- setEncounterDeck [Locations.forestGate]
      run RevealEncounterCard
      chooseAbilityOf pid frodo
      (.exhausted) <$> theOneRing `shouldReturn` True
      (getHeroResources . toId =<< getHeroWithCode frodoCode) `shouldReturn` 0
      -- back in the deck, the card was revealed again
      getEncounterDiscard `shouldReturn` []
      getEncounterDeck `shouldReturn` []
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "is not offered with The One Ring exhausted" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      attachTheOneRing Objectives.theOneRingTheBlackRiders frodo
      ring <- theOneRing
      run $ Exhaust (ObjectiveTarget ring.id)
      run $ PlaceTokens GameSource frodo Resource 1
      _ <- setEncounterDeck [Treacheries.theNecromancersReach]
      run RevealEncounterCard
      offersAbilityOf pid frodo `shouldReturn` False

  describe "Frodo Baggins (The Road Darkens)" do
    it "makes the attack he defends deal no damage, each player raising his threat by 2" . gameTestWith roadDarkensFrodoDeck $ \pid -> do
      frodo <- hero "142001"
      attachTheOneRing Objectives.theOneRingTheRoadDarkens frodo
      run $ PlaceTokens GameSource frodo Resource 1
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      threat <- getThreat pid
      _ <- setEncounterDeck [Locations.forestGate]
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid frodo
      chooseAbilityOf pid frodo
      passActions pid
      getCharacterDamage frodo `shouldReturn` 0
      getThreat pid `shouldReturn` threat + 2

  describe "Sam Gamgee" do
    it "readies with +1 to each stat after engaging an enemy above his threat" . gameTestWith (deckList ["141002", eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      sam <- hero "141002"
      run $ Exhaust sam
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      chooseAbilityOf pid sam
      isExhausted sam `shouldReturn` False
      getWillpower sam `shouldReturn` 4

  describe "Merry and Pippin" do
    it "Merry gets +1 attack per Hobbit hero; Pippin raises each enemy's engagement cost per Hobbit hero" . gameTestWith (deckList ["141003", "141004", eowynCode] testDeck.cards) $ \_ -> do
      merry <- hero "141003"
      eid <- stagedEnemy Enemies.forestSpider
      getAttack merry `shouldReturn` 2
      getEngagementCost eid `shouldReturn` 27

  describe "Fatty Bolger" do
    it "raises your threat by an enemy's threat for it to contribute none this phase" . gameTestWith (deckList ["141005", eowynCode, theodredCode] testDeck.cards) $ \pid -> do
      fatty <- hero "141005"
      eid <- stagedEnemy Enemies.forestSpider
      threat <- getThreat pid
      staging <- getStagingThreat
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid fatty
      chooseTarget pid (EnemyTarget eid)
      getThreat pid `shouldReturn` threat + 2
      getStagingThreat `shouldReturn` staging - 2

  describe "Bill the Pony" do
    it "gives each Hobbit character +1 hit point" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      _ <- putAllyIntoPlay pid Allies.billThePony
      getHitPoints frodo `shouldReturn` 3

  describe "Gandalf (The Road Darkens)" do
    it "plays the top card of the deck once per phase, as if it was in hand" . gameTestWith gandalfDeck $ \pid -> do
      gandalf <- hero gandalfCode
      run $ PlaceTokens GameSource gandalf Resource 4
      top <- putOnTopOfDeck pid Allies.gimliTheTreasonOfSaruman
      run $ BeginPhase PlanningPhase
      chooseTarget pid (CardIdTarget top.id)
      getsGame (any ((== top.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True
      next <- putOnTopOfDeck pid Allies.hamaTheTreasonOfSaruman
      question <- getQuestion pid
      [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (CardIdTarget next.id)

  describe "Fellowship of the Ring" do
    it "gives each hero +1 willpower, and is discarded after a character is destroyed" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      eowyn <- hero eowynCode
      base <- getWillpower eowyn
      _ <- putAttachmentOn pid Attachments.fellowshipOfTheRing frodo
      getWillpower eowyn `shouldReturn` base + 1
      ally <- putAllyIntoPlay pid Allies.farmerMaggot
      run $ Destroy GameSource ally
      getWillpower eowyn `shouldReturn` base

  describe "Phial of Galadriel" do
    it "goes to the victory display, out of the campaign pool, for -4 attack on each enemy engaged with you" . campaignTestIn passageThroughMirkwood ((newCampaignLog "the-lord-of-the-rings") {campaignLogPool = [PoolCard "142015" (Just 0) Nothing]}) frodoDeck $ \pid -> do
      frodo <- hero frodoCode
      phial <- putAttachmentOn pid Attachments.phialOfGaladriel frodo
      eid <- stagedEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid phial
      mods <- getModifiers (EnemyTarget eid)
      mods `shouldSatisfy` elem (AttackModifier (-4))
      campaignLogPool <$> getCampaignLog `shouldReturn` []

  describe "Beyond All Hope" do
    it "keeps treachery attachments off its hero" . gameTestWith frodoDeck $ \pid -> do
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.beyondAllHope eowyn
      getModifiers eowyn >>= (`shouldSatisfy` elem CannotHaveTreacheryAttachments)

  describe "Helm! Helm!" do
    let helmHelmOffered complete pid = do
          eowyn <- hero eowynCode
          run $ PlaceTokens GameSource eowyn Resource 2
          _ <- putAllyIntoPlay pid Allies.hamaTheTreasonOfSaruman
          eid <- stagedEnemy Enemies.forestSpider
          run $ EngageEnemy pid eid
          card <- putCardIntoHand pid Events.helmHelm
          putGame . (phaseL .~ CombatPhase) =<< getGame
          when complete $ run $ CreateEffect $ Effect GameSource GameTarget [EnemyAttacksStepComplete] UntilEndOfPhase
          run $ ActionWindow pid
          question <- getQuestion pid
          pure $ CardIdTarget card.id `elem` [t | Just q <- [question], TargetLabel t _ <- questionChoices q]
    it "is not played before the resolving enemy attacks step is complete" . gameTest $ \pid ->
      helmHelmOffered False pid `shouldReturn` False
    it "is played once the resolving enemy attacks step is complete" . gameTest $ \pid ->
      helmHelmOffered True pid `shouldReturn` True

  describe "Take No Notice" do
    it "costs 1 less per Hobbit hero, and adds 5 to each enemy's engagement cost" . gameTestWith (deckList ["141004", frodoCode, eowynCode] testDeck.cards) $ \pid -> do
      eid <- stagedEnemy Enemies.forestSpider
      -- Pippin's +1 for each of the 2 Hobbit heroes
      getEngagementCost eid `shouldReturn` 27
      _ <- playWith pid "141004" 1 Events.takeNoNotice
      getEngagementCost eid `shouldReturn` 32
