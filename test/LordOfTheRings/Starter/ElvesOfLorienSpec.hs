module LordOfTheRings.Starter.ElvesOfLorienSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard (..))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase
import LordOfTheRings.Sphere (Sphere (Tactics))
import TestImport

elvesTest :: (PlayerId -> GameT ()) -> IO ()
elvesTest = gameTestWith (coreDeck "elves-of-lorien")

celebornCode, galadrielCode, haldirCode, elrondCode :: CardCode
celebornCode = "08001"
galadrielCode = "08112"
haldirCode = "08056"
elrondCode = "04128"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Decline every response offered right now.
passResponses :: PlayerId -> GameT ()
passResponses pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> passResponses pid
    _ -> pure ()

-- | An ally put into play, any responses to it declined.
allyInPlay :: PlayerId -> CardDef -> GameT Target
allyInPlay pid def = do
  a <- putAllyIntoPlay pid def
  passResponses pid
  pure a

allyCard :: Target -> GameT CardId
allyCard = \case
  AllyTarget aid -> allyCardId . toAttrs . fromJustNote "ally" . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> error "not an ally"

alliesInPlay :: GameT [CardCode]
alliesInPlay = getsGame (map toCardCode . toList . (.entities.allies))

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

enemyIn :: CardDef -> GameT EnemyId
enemyIn def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . lookup eid . mapToList . (.entities.enemies))

onTopOfDeck :: PlayerId -> CardDef -> GameT PlayerCard
onTopOfDeck pid def = do
  card <- genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (PlayerCard card :)
  pure card

elrondInPlay :: PlayerId -> GameT Target
elrondInPlay pid = do
  run . PutHeroIntoPlay pid =<< genPlayerCard Heroes.elrond (Just pid)
  hero elrondCode

fund :: CardCode -> Int -> GameT ()
fund code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resources :: CardCode -> GameT Int
resources code = (.resources) <$> getHeroWithCode code

playCard :: PlayerId -> Card -> GameT ()
playCard pid card = chooseTarget pid (CardIdTarget card.id)

spec :: Spec
spec = describe "Elves of Lórien" do
  describe "Silvan Tracker" do
    it "heals 1 damage from a Silvan character that readies in the refresh phase" . elvesTest $ \pid -> do
      _ <- allyInPlay pid Allies.silvanTracker
      haldir <- hero haldirCode
      run $ PlaceTokens GameSource haldir Damage 2
      run $ Exhaust haldir
      run $ BeginPhase RefreshPhase
      chooseAbility pid
      getDamage haldir `shouldReturn` 1

  describe "Mirkwood Runner" do
    it "ignores the defense of the enemy it attacks alone" . elvesTest $ \pid -> do
      runner <- allyInPlay pid Allies.mirkwoodRunner
      eid <- enemyIn Enemies.hillTroll
      run $ AttackWith runner
      run $ ResolveAttack pid eid
      enemyDamage eid `shouldReturn` 2

  describe "Elrond" do
    it "pays for allies of other spheres" . elvesTest $ \pid -> do
      _ <- elrondInPlay pid
      card <- putCardIntoHand pid Allies.galadrielsHandmaiden
      fund elrondCode 2
      run $ BeginPhase PlanningPhase
      playCard pid card
      passResponses pid
      alliesInPlay `shouldReturn` [toCardCode Allies.galadrielsHandmaiden]
      resources elrondCode `shouldReturn` 0

    it "heals 1 more damage on a character another effect heals" . elvesTest $ \pid -> do
      _ <- elrondInPlay pid
      haldir <- hero haldirCode
      run $ PlaceTokens GameSource haldir Damage 3
      run $ HealDamage GameSource haldir 1
      chooseAbility pid
      getDamage haldir `shouldReturn` 1

  describe "Vilya" do
    it "plays the top card of the deck at no cost" . elvesTest $ \pid -> do
      elrond <- elrondInPlay pid
      vilya <- putAttachmentOn pid Attachments.vilya elrond
      _ <- onTopOfDeck pid Allies.greenwoodArcher
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid vilya
      chooseLabel "$label.playIt" pid
      passResponses pid
      alliesInPlay `shouldReturn` [toCardCode Allies.greenwoodArcher]
      isExhausted elrond `shouldReturn` True

    it "moves the card to the bottom of the deck instead" . elvesTest $ \pid -> do
      elrond <- elrondInPlay pid
      vilya <- putAttachmentOn pid Attachments.vilya elrond
      top <- onTopOfDeck pid Allies.greenwoodArcher
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid vilya
      chooseLabel "$label.moveToBottomOfDeck" pid
      (fmap (.id) . lastMay . (.deck)) <$> getPlayer pid `shouldReturn` Just top.id

  describe "Celeborn" do
    it "gives a Silvan ally entering play +1 willpower, attack and defense" . elvesTest $ \pid -> do
      archer <- putAllyIntoPlay pid Allies.greenwoodArcher
      chooseAbilityOf pid =<< hero celebornCode
      passResponses pid
      getWillpower archer `shouldReturn` 1
      getAttack archer `shouldReturn` 3

  describe "Galadriel" do
    it "cannot quest, and allies entering play do not exhaust to quest that round" . elvesTest $ \pid -> do
      archer <- allyInPlay pid Allies.greenwoodArcher
      run $ BeginPhase QuestPhase
      galadriel <- hero galadrielCode
      Just question <- getQuestion pid
      [() | TargetLabel t _ <- questionChoices question, t == galadriel] `shouldBe` []
      chooseTarget pid archer
      isExhausted archer `shouldReturn` False

    it "exhausts to lower a player's threat by 1 and draw a card" . elvesTest $ \pid -> do
      run $ RaiseThreat pid 20
      threat <- (.threat) <$> getPlayer pid
      hand <- length . (.hand) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid =<< hero galadrielCode
      (.threat) <$> getPlayer pid `shouldReturn` (threat - 1)
      length . (.hand) <$> getPlayer pid `shouldReturn` (hand + 1)

  describe "Haldir of Lórien" do
    it "attacks an enemy in the staging area during combat" . elvesTest $ \pid -> do
      eid <- enemyIn Enemies.forestSpider
      run $ BeginPhase CombatPhase
      chooseAbilityOf pid =<< hero haldirCode
      chooseTarget pid eid
      enemyDamage eid `shouldReturn` 2

    it "cannot attack after an enemy engaged him this round" . elvesTest $ \pid -> do
      _ <- enemyIn Enemies.forestSpider
      run . EngageEnemy pid =<< enemyIn Enemies.hillTroll
      run $ BeginPhase CombatPhase
      passActions pid
      haldir <- hero haldirCode
      run $ ActionWindow pid
      offersAbilityOf pid haldir `shouldReturn` False

  describe "Naith Guide" do
    it "lets a hero quest without exhausting this round" . elvesTest $ \pid -> do
      guide <- putAllyIntoPlay pid Allies.naithGuide
      chooseAbilityOf pid guide
      haldir <- hero haldirCode
      chooseTarget pid haldir
      passResponses pid
      run $ BeginPhase QuestPhase
      chooseTarget pid haldir
      isExhausted haldir `shouldReturn` False

  describe "Rúmil" do
    it "deals damage for each ranged character after being played from hand" . elvesTest $ \pid -> do
      eid <- enemyIn Enemies.hillTroll
      run $ EngageEnemy pid eid
      card <- putCardIntoHand pid Allies.rumil
      run $ PlayFromHandAtNoCost pid card.id
      rumil <- getsGame (toTarget . fromJustNote "rumil" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid =<< hero celebornCode
      chooseAbilityOf pid rumil
      chooseTarget pid eid
      -- Rúmil and Haldir
      enemyDamage eid `shouldReturn` 2

  describe "The Tree People" do
    it "returns a Silvan ally to put a Silvan ally from the top of the deck into play" . elvesTest $ \pid -> do
      _ <- allyInPlay pid Allies.greenwoodArcher
      top <- onTopOfDeck pid Allies.mirkwoodRunner
      card <- putCardIntoHand pid Events.theTreePeople
      run $ BeginPhase PlanningPhase
      playCard pid card
      chooseTarget pid (CardIdTarget top.id)
      passResponses pid
      alliesInPlay `shouldReturn` [toCardCode Allies.mirkwoodRunner]
      map toCardCode . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.greenwoodArcher))

  describe "Host of Galadhrim" do
    it "returns each Silvan ally and plays them again at no cost" . elvesTest $ \pid -> do
      archer <- allyInPlay pid Allies.greenwoodArcher
      run $ Exhaust archer
      runner <- allyInPlay pid Allies.mirkwoodRunner
      [archerCard, runnerCard] <- traverse allyCard [archer, runner]
      card <- putCardIntoHand pid Events.hostOfGaladhrim
      fund celebornCode 4
      run $ BeginPhase PlanningPhase
      playCard pid card
      -- the player picks the order: the runner first
      chooseTarget pid (CardIdTarget runnerCard)
      passResponses pid
      alliesInPlay `shouldReturn` [toCardCode Allies.mirkwoodRunner]
      chooseTarget pid (CardIdTarget archerCard)
      passResponses pid
      sort <$> alliesInPlay `shouldReturn` sort [toCardCode Allies.greenwoodArcher, toCardCode Allies.mirkwoodRunner]
      resources celebornCode `shouldReturn` 0
      allies <- getsGame (map toAttrs . toList . (.entities.allies))
      map allyExhausted allies `shouldBe` [False, False]

  describe "O Lórien!" do
    it "lowers the cost of the next Silvan ally by 1" . elvesTest $ \pid -> do
      celeborn <- hero celebornCode
      _ <- putAttachmentOn pid Attachments.oLorien celeborn
      first <- putCardIntoHand pid Allies.naithGuide
      second <- putCardIntoHand pid Allies.naithGuide
      fund celebornCode 3
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid =<< getsGame (toTarget . fromJustNote "o lorien" . headMay . toList . (.entities.attachments))
      playCard pid first
      passResponses pid
      resources celebornCode `shouldReturn` 2
      passActions pid
      run $ BeginPhase PlanningPhase
      playCard pid second
      passResponses pid
      resources celebornCode `shouldReturn` 0

  describe "Bow of the Galadhrim" do
    it "gives +2 attack against an enemy not engaged with you" . elvesTest $ \pid -> do
      haldir <- hero haldirCode
      _ <- putAttachmentOn pid Attachments.bowOfTheGaladhrim haldir
      getAttack haldir `shouldReturn` 4
      eid <- enemyIn Enemies.hillTroll
      run $ AttackWith haldir
      run $ ResolveAttack pid eid
      -- 3 + 1 + 1 attack against 3 defense
      enemyDamage eid `shouldReturn` 2

  describe "Silvan Refugee" do
    it "is discarded after a character leaves play" . elvesTest $ \pid -> do
      _ <- allyInPlay pid Allies.silvanRefugee
      archer <- allyInPlay pid Allies.greenwoodArcher
      run $ Discard archer
      passResponses pid
      alliesInPlay `shouldReturn` []

  describe "Defender of the Naith" do
    it "readies after a Silvan ally you control leaves play" . elvesTest $ \pid -> do
      defender <- allyInPlay pid Allies.defenderOfTheNaith
      archer <- allyInPlay pid Allies.greenwoodArcher
      run $ Exhaust defender
      run $ Discard archer
      chooseAbilityOf pid defender
      isExhausted defender `shouldReturn` False

  describe "Pursuing the Enemy" do
    it "returns a Silvan ally to deal 1 damage to each engaged enemy" . elvesTest $ \pid -> do
      _ <- allyInPlay pid Allies.greenwoodArcher
      eid <- enemyIn Enemies.hillTroll
      run $ EngageEnemy pid eid
      card <- putCardIntoHand pid Events.pursuingTheEnemy
      run $ BeginPhase PlanningPhase
      playCard pid card
      passResponses pid
      enemyDamage eid `shouldReturn` 1
      alliesInPlay `shouldReturn` []

  describe "Daeron's Runes" do
    it "draws 2 cards then discards 1" . elvesTest $ \pid -> do
      card <- putCardIntoHand pid Events.daeronsRunes
      hand <- length . (.hand) <$> getPlayer pid
      run $ BeginPhase PlanningPhase
      playCard pid card
      drawn <- fromJustNote "no hand" . headMay . (.hand) <$> getPlayer pid
      chooseTarget pid (CardIdTarget drawn.id)
      length . (.hand) <$> getPlayer pid `shouldReturn` hand

  describe "Elrond's Counsel" do
    it "gives another character +1 willpower and lowers your threat by 3" . elvesTest $ \pid -> do
      run $ RaiseThreat pid 20
      threat <- (.threat) <$> getPlayer pid
      card <- putCardIntoHand pid Events.elrondsCounsel
      run $ BeginPhase PlanningPhase
      playCard pid card
      haldir <- hero haldirCode
      chooseTarget pid haldir
      getWillpower haldir `shouldReturn` 3
      (.threat) <$> getPlayer pid `shouldReturn` (threat - 3)

  describe "Hands Upon the Bow" do
    it "has a ranged character attack an enemy in the staging area with +1 attack" . elvesTest $ \pid -> do
      eid <- enemyIn Enemies.hillTroll
      card <- putCardIntoHand pid Events.handsUponTheBow
      fund celebornCode 1
      -- no tactics hero in the deck: Celeborn is given the icon
      celeborn <- hero celebornCode
      run $ CreateEffect $ Effect GameSource celeborn [GainResourceIcon Tactics] UntilEndOfPhase
      run $ BeginPhase PlanningPhase
      playCard pid card
      haldir <- hero haldirCode
      chooseTarget pid haldir
      chooseTarget pid eid
      enemyDamage eid `shouldReturn` 1
      getAttack haldir `shouldReturn` 3

  describe "Lembas" do
    it "is discarded to ready its hero and heal 3 damage" . elvesTest $ \pid -> do
      haldir <- hero haldirCode
      lembas <- putAttachmentOn pid Attachments.lembas haldir
      run $ PlaceTokens GameSource haldir Damage 2
      run $ Exhaust haldir
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid lembas
      getDamage haldir `shouldReturn` 0
      isExhausted haldir `shouldReturn` False
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "Nenya" do
    it "adds Galadriel's willpower to another character" . elvesTest $ \pid -> do
      galadriel <- hero galadrielCode
      nenya <- putAttachmentOn pid Attachments.nenya galadriel
      run $ BeginPhase QuestPhase
      chooseAbilityOf pid nenya
      haldir <- hero haldirCode
      chooseTarget pid haldir
      getWillpower haldir `shouldReturn` 6

  describe "Wingfoot" do
    it "readies its hero when a card of the named type is revealed" . elvesTest $ \pid -> do
      haldir <- hero haldirCode
      _ <- putAttachmentOn pid Attachments.wingfoot haldir
      _ <- setEncounterDeck [Enemies.forestSpider]
      run $ BeginPhase QuestPhase
      chooseTarget pid haldir
      chooseAbility pid
      chooseLabel "$label.wingfoot.enemy" pid
      chooseLabel "$label.doneCommitting" pid
      chooseAbility pid
      isExhausted haldir `shouldReturn` False

  describe "Feigned Voices" do
    it "returns a Silvan ally so an engaged enemy cannot attack this phase" . elvesTest $ \pid -> do
      _ <- allyInPlay pid Allies.greenwoodArcher
      eid <- enemyIn Enemies.hillTroll
      run $ EngageEnemy pid eid
      card <- putCardIntoHand pid Events.feignedVoices
      run $ BeginPhase CombatPhase
      playCard pid card
      passResponses pid
      chooseTarget pid eid
      getModifiers eid >>= (`shouldSatisfy` elem (CannotAttackPlayer pid))
      alliesInPlay `shouldReturn` []

  describe "Orophin" do
    it "returns a Silvan ally from the discard pile to hand after entering play" . elvesTest $ \pid -> do
      archer <- genPlayerCard Allies.greenwoodArcher (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . discardL .~ [PlayerCard archer]
      orophin <- putAllyIntoPlay pid Allies.orophin
      chooseAbilityOf pid orophin
      chooseTarget pid (CardIdTarget archer.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem archer.id)

  describe "Galadhrim Minstrel" do
    it "searches the top 5 cards for an event after entering play" . elvesTest $ \pid -> do
      runes <- onTopOfDeck pid Events.daeronsRunes
      minstrel <- putAllyIntoPlay pid Allies.galadhrimMinstrel
      chooseAbilityOf pid minstrel
      chooseTarget pid (CardIdTarget runes.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem runes.id)
