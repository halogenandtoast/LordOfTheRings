module LordOfTheRings.Haradrim.TheBlackSerpentPlayerCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getDefense, getWillpower)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Window (WindowType (DeclaredAsDefender), mkAfter)
import TestImport

beravorCode, eowynCode, theodredCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

give :: CardCode -> Int -> GameT ()
give code n = hero code >>= \h -> run (PlaceTokens GameSource h Resource n)

resourcesOf :: CardCode -> GameT Int
resourcesOf code = getHeroResources . toId =<< getHeroWithCode code

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = do
  g <- getGame
  putGame $ g & playersL . ix pid . threatL .~ n

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  existing <- getsGame (Map.keys . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` existing) . Map.keys . (.entities.enemies))

defeatSideQuest :: PlayerId -> CardDef -> GameT ()
defeatSideQuest pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PutSideQuestIntoPlay pid card
  qid <- getsGame (fromJustNote "no side quest" . fmap toId . find ((== card.id) . questCardId . toAttrs) . toList . (.entities.quests))
  run $ CompleteQuest qid

handIds :: PlayerId -> GameT [CardId]
handIds pid = map (.id) . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "The Black Serpent player cards" do
  describe "Song of Hope" do
    it "spends a resource from its hero for +1 willpower, 3 times a phase" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      song <- putAttachmentOn pid Attachments.songOfHope eowyn
      give eowynCode 4
      run $ ActionWindow pid
      replicateM_ 3 (chooseAbilityOf pid song)
      getWillpower eowyn `shouldReturn` 7
      offersAbilityOf pid song `shouldReturn` False
      resourcesOf eowynCode `shouldReturn` 1

  describe "Burst into Song" do
    it "readies each hero with a Song attachment" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      _ <- putAttachmentOn pid Attachments.songOfHope eowyn
      run $ Exhaust eowyn
      burst <- putCardIntoHand pid Events.burstIntoSong
      give theodredCode 2
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget burst.id)
      heroExhausted . toAttrs <$> getHeroWithCode eowynCode `shouldReturn` False

  describe "Defender of Cair Andros" do
    it "gets +1 defense for each ten of your threat, to +4" . gameTest $ \pid -> do
      defender <- putAllyIntoPlay pid Allies.defenderOfCairAndros
      setThreat pid 29
      run $ CheckWindows []
      getDefense defender `shouldReturn` 2
      setThreat pid 55
      run $ CheckWindows []
      getDefense defender `shouldReturn` 4

    it "deals 1 damage to the attacker as it is declared a defender, with Valour" . gameTest $ \pid -> do
      defender <- putAllyIntoPlay pid Allies.defenderOfCairAndros
      eid <- stagedEnemy Enemies.hillTroll
      setThreat pid 39
      run $ CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
      offersAbilityOf pid defender `shouldReturn` False
      setThreat pid 40
      run $ CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
      chooseAbilityOf pid defender
      getDamage (toTarget eid) `shouldReturn` 1

  describe "Fearless Scout" do
    it "draws a card as it is played" . gameTest $ \pid -> do
      handBefore <- length <$> handIds pid
      _ <- playWith pid eowynCode 1 Attachments.fearlessScout
      chooseTarget pid =<< hero eowynCode
      scout <- getsGame (toTarget . fromJustNote "no attachment" . headMay . toList . (.entities.attachments))
      chooseAbilityOf pid scout
      length <$> handIds pid `shouldReturn` handBefore + 1

  describe "Rally the West" do
    it "gives each hero +1 willpower from the victory display" . gameTest $ \pid -> do
      eowyn <- hero eowynCode
      defeatSideQuest pid Quests.rallyTheWest
      getWillpower eowyn `shouldReturn` 5
      -- a second copy goes to the discard pile, and adds nothing
      defeatSideQuest pid Quests.rallyTheWest
      getWillpower eowyn `shouldReturn` 5

  describe "Dúnedain Pipe" do
    it "places a card from hand on the bottom of the deck to draw a card" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      pipe <- putAttachmentOn pid Attachments.dunedainPipe beravor
      card <- putCardIntoHand pid Events.burstIntoSong
      top <- fmap (.id) . headMay . (.deck) <$> getPlayer pid
      run $ ActionWindow pid
      chooseAbilityOf pid pipe
      chooseTarget pid (CardIdTarget card.id)
      deck <- (.deck) <$> getPlayer pid
      fmap (.id) (lastMay deck) `shouldBe` Just card.id
      hand <- handIds pid
      hand `shouldSatisfy` notElem card.id
      hand `shouldSatisfy` \h -> all (`elem` h) top

  describe "Old Toby" do
    it "draws a card for each Pipe you control and heals each hero with a Pipe" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.dunedainPipe beravor
      run $ PlaceTokens GameSource beravor Damage 2
      toby <- putCardIntoHand pid Events.oldToby
      give beravorCode 2
      handBefore <- length <$> handIds pid
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget toby.id)
      getCharacterDamage beravor `shouldReturn` 1
      -- Old Toby left the hand, one card came in
      length <$> handIds pid `shouldReturn` handBefore

  describe "Southron Refugee" do
    it "exhausts to make the next Harad ally cost 1 less" . gameTest $ \pid -> do
      refugee <- putAllyIntoPlay pid Allies.southronRefugee
      firyal <- putCardIntoHand pid Allies.firyal
      give beravorCode 4
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid refugee
      chooseTarget pid (CardIdTarget firyal.id)
      getsGame (any ((== firyal.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True
      resourcesOf beravorCode `shouldReturn` 0
