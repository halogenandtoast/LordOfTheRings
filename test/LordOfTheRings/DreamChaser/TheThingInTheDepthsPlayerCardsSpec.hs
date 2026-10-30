-- | The player cards of The Thing in the Depths.
module LordOfTheRings.DreamChaser.TheThingInTheDepthsPlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getHitPoints, getWillpower)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (AfterStagingStep))
import LordOfTheRings.Phase
import LordOfTheRings.Window (WindowType (QuestedSuccessfully, Revealed), mkAfter)
import TestImport

beravorCode, eowynCode, theodredCode, gimliCode, legolasCode, lanwynCode, frodoCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
gimliCode = "01004"
legolasCode = "01005"
lanwynCode = "12030"
frodoCode = "02025"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

resources :: CardCode -> GameT Int
resources code = (.resources) <$> getHeroWithCode code

giveResources :: CardCode -> Int -> GameT ()
giveResources code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . lookup hid . mapToList <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

isCommitted :: Target -> GameT Bool
isCommitted = \case
  AllyTarget aid -> maybe False (allyCommitted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

allyProgress :: Target -> GameT Int
allyProgress = \case
  AllyTarget aid -> maybe 0 (countTokens Progress . allyTokens . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure 0

staged :: CardDef -> GameT EnemyId
staged def = do
  was <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` was) . map toId . toList . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

attachmentHost :: CardDef -> GameT (Maybe Placement)
attachmentHost def = getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.attachments))

withHeroes :: [CardCode] -> (PlayerId -> GameT ()) -> IO ()
withHeroes codes = gameTestWith testDeck {heroes = codes}

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices
    <$> getQuestion pid

spec :: Spec
spec = describe "The Thing in the Depths player cards" do
  describe "Lanwyn" do
    it "readies or gets +2 willpower after a card with surge is revealed, twice a phase" . withHeroes [lanwynCode, eowynCode, theodredCode] $ \pid -> do
      lanwyn <- hero lanwynCode
      run $ Exhaust lanwyn
      crows <- staged Enemies.easternCrows
      let surge = run $ CheckWindows [mkAfter $ Revealed (EnemyTarget crows)]
      surge
      chooseAbilityOf pid lanwyn
      chooseLabel "$label.lanwyn.ready" pid
      isExhausted lanwyn `shouldReturn` False
      surge
      chooseAbilityOf pid lanwyn
      getWillpower lanwyn `shouldReturn` 4
      surge
      offersAbilityOf pid lanwyn `shouldReturn` False

  describe "Eldahir" do
    it "spends 1 lore to look at a shadow card, getting +2 defense for one with an effect" . gameTest $ \pid -> do
      eid <- staged Enemies.forestSpider
      run $ EngageEnemy pid eid
      run . AttachShadowCard eid =<< genEncounterCard Enemies.eastBightPatrol
      eldahir <- putAllyIntoPlay pid Allies.eldahir
      giveResources beravorCode 1
      run $ ActionWindow pid
      chooseAbilityOf pid eldahir
      chooseLabel "$label.continue" pid
      getDefense eldahir `shouldReturn` 4
      resources beravorCode `shouldReturn` 0

  describe "Captain's Wisdom" do
    it "exhausts a Noble hero in the resource phase to add 2 resources to its pool" . gameTest $ \pid -> do
      wisdom <- putCardIntoHand pid Events.captainsWisdom
      run $ BeginPhase ResourcePhase
      chooseTarget pid (CardIdTarget wisdom.id)
      theodred <- hero theodredCode
      chooseTarget pid theodred
      resources theodredCode `shouldReturn` 3
      isExhausted theodred `shouldReturn` True

    it "is not played outside the resource phase" . gameTest $ \pid -> do
      wisdom <- putCardIntoHand pid Events.captainsWisdom
      run $ BeginPhase PlanningPhase
      offersCard pid wisdom `shouldReturn` False

  describe "Master Ironsmith" do
    it "attaches a Weapon or Armor from hand at no cost to the hero who paid for it alone" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      raiment <- putCardIntoHand pid Attachments.raimentOfWar
      _ <- playWith pid gimliCode 3 Allies.masterIronsmith
      smith <- getsGame (toTarget . fromJustNote "no ironsmith" . headMay . toList . (.entities.allies))
      chooseAbilityOf pid smith
      chooseTarget pid (CardIdTarget raiment.id)
      gimli <- hero gimliCode
      attachmentHost Attachments.raimentOfWar `shouldReturn` Just (AttachedTo gimli)
      resources gimliCode `shouldReturn` 0

    it "is not offered when two heroes paid for it" . withHeroes [gimliCode, legolasCode, theodredCode] $ \pid -> do
      _ <- putCardIntoHand pid Attachments.raimentOfWar
      giveResources legolasCode 1
      _ <- playWith pid gimliCode 2 Allies.masterIronsmith
      gimli <- hero gimliCode
      replicateM_ 2 $ chooseTarget pid gimli
      resources legolasCode `shouldReturn` 0
      smith <- getsGame (toTarget . fromJustNote "no ironsmith" . headMay . toList . (.entities.allies))
      offersAbilityOf pid smith `shouldReturn` False

  describe "Raiment of War" do
    it "gives a Warrior +1 attack, +1 defense and +2 hit points" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      gimli <- hero gimliCode
      _ <- putAttachmentOn pid Attachments.raimentOfWar gimli
      getAttack gimli `shouldReturn` 3
      getDefense gimli `shouldReturn` 3
      getHitPoints gimli `shouldReturn` 7

    it "counts as 2 restricted attachments" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      gimli <- hero gimliCode
      _ <- putAttachmentOn pid Attachments.raimentOfWar gimli
      blade <- genPlayerCard Attachments.bladeOfGondolin (Just pid)
      run $ PlayAttachmentOn pid blade gimli
      getQuestion pid >>= (`shouldSatisfy` maybe False (\q -> length (questionChoices q) == 2))

  describe "Sam Gamgee" do
    it "costs 2 less while you control Frodo Baggins" . withHeroes [frodoCode, eowynCode, theodredCode] $ \pid -> do
      _ <- playWith pid eowynCode 1 Allies.samGamgee
      map toCardCode . toList <$> getsGame (.entities.allies) `shouldReturn` [toCardCode Allies.samGamgee]

    it "spends 1 spirit to ready after a player raises his threat, with +1 to each stat for the round" . gameTest $ \pid -> do
      sam <- putAllyIntoPlay pid Allies.samGamgee
      run $ Exhaust sam
      giveResources eowynCode 1
      run $ RaiseThreat pid 1
      chooseAbilityOf pid sam
      isExhausted sam `shouldReturn` False
      getWillpower sam `shouldReturn` 3
      getAttack sam `shouldReturn` 2
      getDefense sam `shouldReturn` 2
      resources eowynCode `shouldReturn` 0

  describe "Elevenses" do
    it "readies questing Hobbits, removes them from the quest and lowers threat by as many" . gameTest $ \pid -> do
      sam <- putAllyIntoPlay pid Allies.samGamgee
      brandybuck <- putAllyIntoPlay pid Allies.curiousBrandybuck
      run $ CommitToQuest sam
      run $ CommitToQuest brandybuck
      putGame . (phaseL .~ QuestPhase) =<< getGame
      run $ CreateEffect $ Effect GameSource GameTarget [AfterStagingStep] UntilEndOfPhase
      elevenses <- putCardIntoHand pid Events.elevenses
      giveResources eowynCode 1
      putGame . (playersL . ix pid . threatL .~ 20) =<< getGame
      was <- threat pid
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget elevenses.id)
      -- the first offered, then the other
      replicateM_ 2 $ chooseOptionMatching "choose a Hobbit" pid \case
        TargetLabel {} -> True
        _ -> False
      threat pid `shouldReturn` was - 2
      isExhausted sam `shouldReturn` False
      isCommitted sam `shouldReturn` False
      isCommitted brandybuck `shouldReturn` False

    it "is not played before the staging step is over" . gameTest $ \pid -> do
      sam <- putAllyIntoPlay pid Allies.samGamgee
      run $ CommitToQuest sam
      putGame . (phaseL .~ QuestPhase) =<< getGame
      elevenses <- putCardIntoHand pid Events.elevenses
      giveResources eowynCode 1
      run $ ActionWindow pid
      offersCard pid elevenses `shouldReturn` False

  describe "Mirkwood Explorer" do
    it "gains progress questing successfully, and exhausts to move it to a location" . gameTest $ \pid -> do
      explorer <- putAllyIntoPlay pid Allies.mirkwoodExplorer
      run $ CommitToQuest explorer
      run $ CheckWindows [mkAfter QuestedSuccessfully]
      chooseAbilityOf pid explorer
      allyProgress explorer `shouldReturn` 1
      run $ Ready explorer
      lid <- stagedLocation Locations.mountainsOfMirkwood
      run $ ActionWindow pid
      chooseAbilityOf pid explorer
      allyProgress explorer `shouldReturn` 0
      (.progress) <$> getLocation lid `shouldReturn` 1

  describe "Guarded Ceaselessly" do
    it "exhausts a Ranger or Scout to give the location -2 threat for the phase" . gameTest $ \pid -> do
      lid <- stagedLocation Locations.mountainsOfMirkwood
      _ <- putAttachmentOn pid Attachments.guardedCeaselessly (LocationTarget lid)
      was <- getLocationThreat lid
      guarded <- getsGame (toTarget . fromJustNote "no attachment" . headMay . toList . (.entities.attachments))
      run $ ActionWindow pid
      chooseAbilityOf pid guarded
      beravor <- hero beravorCode
      chooseTarget pid beravor
      getLocationThreat lid `shouldReturn` was - 2
      isExhausted beravor `shouldReturn` True
