module LordOfTheRings.Hobbit.PlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getWillpower)
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Matcher (AllyMatcher (AllyWithTitle), EnemyMatcher (AnyEnemy), dwarfCount, getEnemyDefense, select, selectAny)
import LordOfTheRings.Modifier (ModifierType (StagingStepBegun))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase, QuestPhase, RefreshPhase, ResourcePhase))
import LordOfTheRings.Sphere (Sphere (Spirit))
import TestImport

eowynCode, theodredCode, beravorCode :: CardCode
eowynCode = "01007"
theodredCode = "01002"
beravorCode = "01012"

code :: CardDef -> CardCode
code = toCardCode

with :: [CardDef] -> DeckList
with heroes = deckList (map code heroes) testDeck.cards

hero :: CardCode -> GameT Target
hero c = toTarget <$> getHeroWithCode c

resources :: CardCode -> GameT Int
resources c = (.resources) <$> getHeroWithCode c

give :: CardCode -> Int -> GameT ()
give c n = hero c >>= \h -> run (PlaceTokens GameSource h Resource n)

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . fmap enemyId . find ((== card.id) . enemyCardId) . map toAttrs . toList . (.entities.enemies))

enemyDamage :: EnemyId -> GameT Int
enemyDamage eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . Map.lookup eid . (.entities.enemies))

enemyInPlay :: EnemyId -> GameT Bool
enemyInPlay eid = getsGame (Map.member eid . (.entities.enemies))

atPhase :: Phase -> GameT ()
atPhase phase = putGame . (phaseL .~ phase) =<< getGame

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices <$> getQuestion pid

spec :: Spec
spec = describe "The Hobbit player cards" do
  describe "Bilbo Baggins (Over Hill and Under Hill)" do
    let bilbo = code Heroes.bilboBagginsOverHillAndUnderHill
    it "loses the game as he leaves play" . gameTestWith (with [Heroes.bilboBagginsOverHillAndUnderHill]) $ \_ -> do
      run . Discard =<< hero bilbo
      getsGame (.outcome) `shouldReturn` Just Lost

    it "gains no resources from player card effects" . gameTestWith (with [Heroes.bilboBagginsOverHillAndUnderHill, Heroes.thorinOakenshield]) $ \pid -> do
      gloin <- putAllyIntoPlay pid Allies.gloin
      b <- hero bilbo
      for_ [aid | AllyTarget aid <- [gloin]] \aid -> run $ PlaceTokens (AllySource aid) b Resource 2
      resources bilbo `shouldReturn` 0

  describe "Bilbo Baggins (On the Doorstep)" do
    let bilbo = code Heroes.bilboBagginsOnTheDoorstep
        deck = with [Heroes.bilboBagginsOnTheDoorstep, Heroes.balin]
    it "gains resources from a treasure, Bilbo's Magic Ring, but from no other card" . gameTestWith deck $ \pid -> do
      b <- hero bilbo
      gloin <- putAllyIntoPlay pid Allies.gloin
      for_ [aid | AllyTarget aid <- [gloin]] \aid -> run $ PlaceTokens (AllySource aid) b Resource 2
      resources bilbo `shouldReturn` 0
      ring <- putAttachmentOn pid Attachments.bilbosMagicRing b
      threat <- (.threat) <$> getPlayer pid
      run $ ActionWindow pid
      chooseAbilityOf pid ring
      resources bilbo `shouldReturn` 1
      (.threat) <$> getPlayer pid `shouldReturn` threat + 1

    it "spends a Baggins resource to search his deck for a treasure" . gameTestWith (deckList [bilbo] ((code Attachments.mithrilShirtTreasure, 1) : testDeck.cards)) $ \pid -> do
      give bilbo 1
      run $ ActionWindow pid
      chooseAbilityOf pid =<< hero bilbo
      Just shirt <- find ((== code Attachments.mithrilShirtTreasure) . toCardCode) . (.deck) <$> getPlayer pid
      chooseTarget pid (CardIdTarget shirt.id)
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [shirt.id]
      resources bilbo `shouldReturn` 0

  describe "Burglar Baggins" do
    it "is paid for from the pool of a Bilbo Baggins another player controls" . twoPlayerTest (with [Heroes.bilboBagginsOverHillAndUnderHill, Heroes.balin]) $ \one two -> do
      let bilbo = code Heroes.bilboBagginsOverHillAndUnderHill
      b <- getHeroWithCode bilbo
      when (b.owner == one) $ run $ ChangeHeroController b.id two
      card <- putCardIntoHand one Events.burglarBaggins
      give bilbo 1
      run $ ActionWindow one
      chooseTarget one (CardIdTarget card.id)
      resources bilbo `shouldReturn` 0
      (getWillpower =<< hero bilbo) `shouldReturn` 3
      (getAttack =<< hero bilbo) `shouldReturn` 3

  describe "Bombur, Thorin Oakenshield and Óin" do
    let deck = with [Heroes.thorinOakenshield, Heroes.bombur, Heroes.oin]
    it "counts Bombur as two Dwarves" . gameTestWith deck $ \pid -> do
      putGame =<< preloadModifiers =<< getGame
      dwarfCount pid `shouldReturn` 4
      (getAttack =<< hero (code Heroes.oin)) `shouldReturn` 1
      _ <- putAllyIntoPlay pid Allies.bifur
      dwarfCount pid `shouldReturn` 5
      (getAttack =<< hero (code Heroes.oin)) `shouldReturn` 2

    it "gives Thorin an additional resource with 5 Dwarves" . gameTestWith deck $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.bifur
      run $ DoPhase ResourcePhase
      resources (code Heroes.thorinOakenshield) `shouldReturn` 2
      resources (code Heroes.bombur) `shouldReturn` 1

  describe "Desperate Alliance" do
    it "lends a hero and its resources until the end of the phase, its player not eliminated meanwhile" . twoPlayerTest (with [Heroes.balin, Heroes.oin]) $ \one two -> do
      heroes <- filter ((== one) . (.owner)) . toList <$> getsGame (.entities.heroes)
      -- each player has a Spirit hero to play it, Éowyn or Óin, and lends another
      let lent = fromJustNote "no other hero" $ find ((/= Spirit) . (.sphere)) heroes
      run $ PlaceTokens GameSource (toTarget lent) Resource 2
      card <- putCardIntoHand one Events.desperateAlliance
      atPhase PlanningPhase
      run $ ActionWindow one
      chooseTarget one (CardIdTarget card.id)
      chooseTarget one (toTarget lent)
      getHeroWithCode (toCardCode lent) >>= \h -> (h.owner, h.resources) `shouldBe` (two, 2)
      for_ [h | h <- heroes, h.id /= lent.id] (run . Discard . toTarget)
      run CheckEliminations
      (.eliminated) <$> getPlayer one `shouldReturn` False
      passActions one
      run $ EndPhase PlanningPhase
      (.owner) <$> getHeroWithCode (toCardCode lent) `shouldReturn` one

  describe "Expecting Mischief" do
    it "deals 2 damage to the first enemy revealed this phase, and no other" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Enemies.forestSpider, Enemies.forestSpider]
      card <- putCardIntoHand pid Events.expectingMischief
      give beravorCode 1
      atPhase QuestPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget card.id)
      passActions pid
      run RevealEncounterCard
      resolveRevealed
      run RevealEncounterCard
      resolveRevealed
      damage <- traverse enemyDamage =<< select AnyEnemy
      sort damage `shouldBe` [0, 2]

    it "cannot be played once the staging step has begun" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Events.expectingMischief
      give beravorCode 1
      atPhase QuestPhase
      run $ CreateEffect $ Effect GameSource GameTarget [StagingStepBegun] UntilEndOfPhase
      run $ ActionWindow pid
      offersCard pid card `shouldReturn` False

  describe "Great Yew Bow and Bard the Bowman" do
    it "attacks an enemy in the staging area alone, at -2 defense for Bard's ranged attack" . gameTestWith (with [Heroes.bardTheBowman]) $ \pid -> do
      bard <- hero (code Heroes.bardTheBowman)
      bow <- putAttachmentOn pid Attachments.greatYewBow bard
      spider <- stagedEnemy Enemies.forestSpider
      atPhase CombatPhase
      run $ ActionWindow pid
      chooseAbilityOf pid bow
      passActions pid
      getEnemyDefense spider `shouldReturn` 0
      -- Bard's 3 attack against no defense
      enemyDamage spider `shouldReturn` 3
      exhausted <$> getHeroWithCode (code Heroes.bardTheBowman) `shouldReturn` True

  describe "Straight Shot" do
    it "exhausts a Weapon to discard a non-unique enemy with 0 defense" . gameTestWith (with [Heroes.bardTheBowman]) $ \pid -> do
      bard <- hero (code Heroes.bardTheBowman)
      _ <- putAttachmentOn pid Attachments.bladeOfGondolin bard
      bats <- stagedEnemy Enemies.blackForestBats
      spider <- stagedEnemy Enemies.forestSpider
      _ <- playWith pid (code Heroes.bardTheBowman) 0 Events.straightShot
      enemyInPlay bats `shouldReturn` False
      enemyInPlay spider `shouldReturn` True

  describe "To me! O my kinsfolk!" do
    it "puts a Dwarf ally into play from the discard pile, then on the bottom of the deck" . gameTestWith (with [Heroes.thorinOakenshield]) $ \pid -> do
      dwalin <- genPlayerCard Allies.dwalin (Just pid)
      run $ PutInDiscard dwalin
      _ <- playWith pid (code Heroes.thorinOakenshield) 1 Events.toMeOMyKinsfolk
      selectAny (AllyWithTitle "Dwalin") `shouldReturn` True
      passActions pid
      run $ EndPhase PlanningPhase
      selectAny (AllyWithTitle "Dwalin") `shouldReturn` False
      fmap (.id) . lastMay . (.deck) <$> getPlayer pid `shouldReturn` Just dwalin.id

  describe "Balin" do
    it "pays a resource to cancel a shadow effect, and the enemy is dealt another" . gameTestWith (with [Heroes.balin]) $ \pid -> do
      balin <- hero (code Heroes.balin)
      give (code Heroes.balin) 1
      eid <- stagedEnemy Enemies.blackForestBats
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Enemies.dolGuldurOrcs, Enemies.forestSpider]
      run $ DealShadowCard eid
      run $ DefendWith pid eid balin
      passActions pid
      chooseAbilityOf pid balin
      passActions pid
      resources (code Heroes.balin) `shouldReturn` 0
      -- the enemy holds both shadow cards, the second dealt as the first was canceled
      getsGame (maybe [] (map toCardCode . enemyShadowCards . toAttrs) . Map.lookup eid . (.entities.enemies))
        >>= (`shouldSatisfy` ((== sort (map code [Enemies.dolGuldurOrcs, Enemies.forestSpider])) . sort))

  describe "Gandalf (Over Hill and Under Hill)" do
    it "stays at the end of the refresh phase for 2 threat" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.gandalfHobbit
      threat <- (.threat) <$> getPlayer pid
      atPhase RefreshPhase
      run $ EndPhase RefreshPhase
      chooseLabel "$label.gandalfHobbit.raiseThreat" pid
      (.threat) <$> getPlayer pid `shouldReturn` threat + 2
      selectAny (AllyWithTitle "Gandalf") `shouldReturn` True

    it "is discarded otherwise" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.gandalfHobbit
      atPhase RefreshPhase
      run $ EndPhase RefreshPhase
      chooseLabel "$label.gandalfHobbit.discard" pid
      selectAny (AllyWithTitle "Gandalf") `shouldReturn` False

  describe "Spare Hood and Cloak" do
    it "exhausts with its character to ready another, then moves to it" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      theodred <- hero theodredCode
      cloak <- putAttachmentOn pid Attachments.spareHoodAndCloak eowyn
      run $ Exhaust theodred
      run $ ActionWindow pid
      chooseAbilityOf pid cloak
      chooseTarget pid theodred
      exhausted <$> getHeroWithCode theodredCode `shouldReturn` False
      exhausted <$> getHeroWithCode eowynCode `shouldReturn` True
      getsGame (fmap (.placement) . Map.lookup (fromJustNote "no cloak" $ attachmentIdOf cloak) . (.entities.attachments))
        `shouldReturn` Just (AttachedTo theodred)

exhausted :: Hero -> Bool
exhausted = heroExhausted . toAttrs

attachmentIdOf :: Target -> Maybe AttachmentId
attachmentIdOf = \case
  AttachmentTarget aid -> Just aid
  _ -> Nothing
