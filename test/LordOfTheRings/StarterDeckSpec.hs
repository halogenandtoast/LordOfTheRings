module LordOfTheRings.StarterDeckSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally (Ally, AllyAttrs (..))
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard (..), genPlayerCard)
import LordOfTheRings.Decklist (sharedHeroes)
import LordOfTheRings.Sphere (Sphere (Spirit))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import TestImport

-- Starter Set Leadership: Aragorn, Théodred and Glóin.
aragornCode, theodredCode, gloinCode :: CardCode
aragornCode = "01001"
theodredCode = "01002"
gloinCode = "01003"

starterTest :: (PlayerId -> GameT ()) -> IO ()
starterTest = gameTestWith starterDeck

-- | Put a card in hand, give Aragorn the resources for it and play it in the planning phase.
playFromHand :: PlayerId -> CardDef -> Int -> GameT Card
playFromHand pid def resources = do
  card <- putCardIntoHand pid def
  aragorn <- getHeroWithCode aragornCode
  run $ PlaceTokens GameSource (toTarget aragorn) Resource resources
  run $ BeginPhase PlanningPhase
  chooseTarget pid card
  pure card

allyInPlay :: PlayerId -> CardDef -> GameT Ally
allyInPlay pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PlayAlly pid card
  whenJustM (getQuestion pid) \_ -> chooseLabel "$label.pass" pid
  getsGame (fromJustNote "ally not in play" . find ((== pcId card) . allyCardId . toAttrs) . toList . (.entities.allies))

allies :: GameT [Ally]
allies = getsGame (toList . (.entities.allies))

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

spec :: Spec
spec = describe "Starter deck" do
  describe "sharedHeroes" do
    it "finds hero titles more than one deck brings" $ liftIO @IO do
      let deck sphere = fromJustNote (unpack sphere) $ lookup (sphere :: Text) coreStarterDecks
      sharedHeroes [deck "leadership", deck "tactics", deck "spirit", deck "lore"] `shouldBe` []
      sort (sharedHeroes [deck "leadership", deck "lore", deck "leadership"]) `shouldBe` ["Aragorn", "Glóin", "Théodred"]

  describe "core starter decks" do
    for_ coreStarterDecks \(sphere, deck) ->
      it ("deals the " <> unpack sphere <> " deck with three heroes") . gameTestWith deck $ \_ -> do
        heroes <- getsGame (length . (.entities.heroes))
        heroes `shouldBe` 3

  describe "Théodred" do
    it "adds 1 resource to a committed hero after committing" . starterTest $ \pid -> do
      aragorn <- getHeroWithCode aragornCode
      theodred <- getHeroWithCode theodredCode
      run $ BeginPhase QuestPhase
      chooseTarget pid aragorn
      chooseTarget pid theodred
      chooseAbility pid
      chooseTarget pid aragorn
      (.resources) <$> getHeroWithCode aragornCode `shouldReturn` 1

  describe "Faramir" do
    it "exhausts to give each of your characters +1 willpower" . starterTest $ \pid -> do
      _ <- playFromHand pid Allies.faramir 4
      chooseAbility pid
      aragorn <- getHeroWithCode aragornCode
      getWillpower (toTarget aragorn) `shouldReturn` 3

  describe "Son of Arnor" do
    it "engages an enemy from the staging area after entering play" . starterTest $ \pid -> do
      flipFirstQuest pid
      enemy <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
      _ <- playFromHand pid Allies.sonOfArnor 3
      chooseAbility pid
      chooseTarget pid (toId enemy)
      enemy' <- getsGame (fromJustNote "gone" . Map.lookup (toId enemy) . (.entities.enemies))
      enemyPlacement (toAttrs enemy') `shouldBe` EngagedWith pid

  describe "Longbeard Orc Slayer" do
    it "deals 1 damage to each Orc enemy after entering play" . starterTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.goblinSniper
      _ <- playFromHand pid Allies.longbeardOrcSlayer 4
      chooseAbility pid
      getsGame (map (.damage) . toList . (.entities.enemies)) `shouldReturn` [1]

  describe "Brok Ironfist" do
    it "enters play from hand after a Dwarf hero you control leaves play" . starterTest $ \pid -> do
      brok <- putCardIntoHand pid Allies.brokIronfist
      gloin <- getHeroWithCode gloinCode
      run $ Discard (toTarget gloin)
      chooseTarget pid brok
      map toCardCode <$> allies `shouldReturn` ["01019"]

  describe "Gandalf" do
    it "can reduce your threat by 5 after entering play" . starterTest $ \pid -> do
      flipFirstQuest pid
      threat <- (.threat) <$> getPlayer pid
      _ <- playFromHand pid Allies.gandalf 5
      chooseAbility pid
      chooseLabel "$label.gandalf.threat" pid
      (.threat) <$> getPlayer pid `shouldReturn` (threat - 5)

    it "is discarded at the end of the round" . starterTest $ \pid -> do
      _ <- allyInPlay pid Allies.gandalf
      run EndRound
      length <$> allies `shouldReturn` 0

  describe "Dúnedain Mark" do
    it "gives +1 attack and moves to another hero for 1 resource" . starterTest $ \pid -> do
      _ <- playFromHand pid Attachments.dunedainMark 2
      aragorn <- getHeroWithCode aragornCode
      theodred <- getHeroWithCode theodredCode
      chooseTarget pid aragorn
      getAttack (toTarget aragorn) `shouldReturn` 4
      chooseAbility pid
      chooseTarget pid theodred
      getsGame (map (.placement) . toList . (.entities.attachments)) `shouldReturn` [AttachedTo (toTarget theodred)]
      (.resources) <$> getHeroWithCode aragornCode `shouldReturn` 0

  describe "Celebrían's Stone" do
    it "gives Aragorn a Spirit resource icon" . starterTest $ \pid -> do
      _ <- playFromHand pid Attachments.celebriansStone 3
      chooseTarget pid =<< getHeroWithCode aragornCode
      (`shouldReturn` 1) . resourcesInSphere Spirit =<< getPlayer pid

  describe "Ever Vigilant" do
    it "readies an ally" . starterTest $ \pid -> do
      guard' <- allyInPlay pid Allies.guardOfTheCitadel
      run $ Exhaust (toTarget guard')
      _ <- playFromHand pid Events.everVigilant 1
      chooseTarget pid (toId guard')
      map (allyExhausted . toAttrs) <$> allies `shouldReturn` [False]

  describe "Common Cause" do
    it "exhausts a hero you control to ready a different hero" . starterTest $ \pid -> do
      theodred <- getHeroWithCode theodredCode
      run $ Exhaust (toTarget theodred)
      _ <- playFromHand pid Events.commonCause 0
      chooseTarget pid =<< getHeroWithCode aragornCode
      chooseTarget pid theodred
      exhausted aragornCode `shouldReturn` True
      exhausted theodredCode `shouldReturn` False

  describe "For Gondor!" do
    it "gives all characters +1 attack and Gondor characters +1 defense" . starterTest $ \pid -> do
      guard' <- allyInPlay pid Allies.guardOfTheCitadel
      _ <- playFromHand pid Events.forGondor 2
      aragorn <- getHeroWithCode aragornCode
      getAttack (toTarget aragorn) `shouldReturn` 4
      getDefense (toTarget aragorn) `shouldReturn` 2
      getDefense (toTarget guard') `shouldReturn` 1

  describe "Sneak Attack" do
    it "puts an ally into play until the end of the phase" . starterTest $ \pid -> do
      guardCard <- putCardIntoHand pid Allies.guardOfTheCitadel
      _ <- playFromHand pid Events.sneakAttack 1
      chooseTarget pid guardCard
      map toCardCode <$> allies `shouldReturn` ["01013"]
      chooseLabel "$label.donePlanning" pid
      run $ EndPhase PlanningPhase
      length <$> allies `shouldReturn` 0
      handCodes pid >>= (`shouldSatisfy` elem "01013")

  describe "Valiant Sacrifice" do
    it "draws 2 cards after an ally leaves play" . starterTest $ \pid -> do
      guard' <- allyInPlay pid Allies.guardOfTheCitadel
      sacrifice <- putCardIntoHand pid Events.valiantSacrifice
      aragorn <- getHeroWithCode aragornCode
      run $ PlaceTokens GameSource (toTarget aragorn) Resource 1
      before <- length <$> handCodes pid
      run $ Discard (toTarget guard')
      chooseTarget pid sacrifice
      length <$> handCodes pid `shouldReturn` (before - 1 + 2)

  describe "Grim Resolve" do
    it "readies all characters" . starterTest $ \pid -> do
      for_ [aragornCode, theodredCode] \code -> run . Exhaust . toTarget =<< getHeroWithCode code
      _ <- playFromHand pid Events.grimResolve 5
      exhausted aragornCode `shouldReturn` False
      exhausted theodredCode `shouldReturn` False
