-- | The player cards of Temple of the Deceived.
module LordOfTheRings.DreamChaser.TempleOfTheDeceivedPlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (AfterEngagementChecks, Blank, EnemyEngagedThisPhase))
import LordOfTheRings.Phase
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Window (WindowType (CommittedToQuest, DefendedAttack), mkAfter)
import TestImport

beravorCode, eowynCode, theodredCode, legolasCode, glorfindelCode, elfhelmCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
legolasCode = "01005"
glorfindelCode = "01011"
elfhelmCode = "12057"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

resources :: CardCode -> GameT Int
resources code = (.resources) <$> getHeroWithCode code

giveResources :: CardCode -> Int -> GameT ()
giveResources code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . lookup hid . mapToList <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

staged :: CardDef -> GameT EnemyId
staged def = do
  was <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` was) . map toId . toList . (.entities.enemies))

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- staged def
  run $ EngageEnemy pid eid
  pure eid

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

shadowCount :: EnemyId -> GameT Int
shadowCount eid = length . (.shadowCards) <$> getEnemy eid

withHeroes :: [CardCode] -> (PlayerId -> GameT ()) -> IO ()
withHeroes codes = gameTestWith testDeck {heroes = codes}

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices
    <$> getQuestion pid

-- | In the encounter phase, once the engagement checks are made.
afterEngagementChecks :: GameT ()
afterEngagementChecks = do
  putGame . (phaseL .~ EncounterPhase) =<< getGame
  run $ CreateEffect $ Effect GameSource GameTarget [AfterEngagementChecks] UntilEndOfPhase

spec :: Spec
spec = describe "Temple of the Deceived player cards" do
  describe "Elfhelm" do
    it "gives each hero with a Mount a bonus by its sphere" . withHeroes [elfhelmCode, eowynCode, theodredCode] $ \pid -> do
      eowyn <- hero eowynCode
      theodred <- hero theodredCode
      _ <- putAttachmentOn pid Attachments.armoredDestrier eowyn
      _ <- putAttachmentOn pid Attachments.armoredDestrier theodred
      getWillpower eowyn `shouldReturn` 5
      getDefense theodred `shouldReturn` 2

  describe "Ceorl" do
    it "spends 1 spirit to ready, then may pass to another player" . twoPlayerTest testDeck $ \one two -> do
      ceorl <- putAllyIntoPlay one Allies.ceorl
      run $ Exhaust ceorl
      eowyn <- heroOf one eowynCode
      run $ PlaceTokens GameSource eowyn Resource 1
      run $ ActionWindow one
      chooseAbilityOf one ceorl
      chooseTarget one two
      isExhausted ceorl `shouldReturn` False
      getsGame ((placementController . (.placement) =<<) . find ((== ceorl) . toTarget) . toList . (.entities.allies)) `shouldReturn` Just two

  describe "Armored Destrier" do
    it "readies the hero after it defends and discards a shadow card from another engaged enemy" . gameTest $ \pid -> do
      theodred <- hero theodredCode
      destrier <- putAttachmentOn pid Attachments.armoredDestrier theodred
      attacker <- engaged pid Enemies.forestSpider
      other <- engaged pid Enemies.forestSpider
      run . AttachShadowCard other =<< genEncounterCard Enemies.eastBightPatrol
      run $ Exhaust theodred
      run $ CheckWindows [mkAfter $ DefendedAttack theodred attacker]
      chooseAbilityOf pid destrier
      isExhausted theodred `shouldReturn` False
      shadowCount other `shouldReturn` 0

  describe "Déorwine" do
    it "spends 1 leadership to cancel a shadow effect while defending" . gameTest $ \pid -> do
      eid <- engaged pid Enemies.forestSpider
      run . AttachShadowCard eid =<< genEncounterCard Enemies.eastBightPatrol
      deorwine <- putAllyIntoPlay pid Allies.deorwine
      giveResources theodredCode 1
      was <- getEnemyAttack eid
      run $ ResolveShadow eid pid [deorwine]
      chooseAbilityOf pid deorwine
      getEnemyAttack eid `shouldReturn` was
      resources theodredCode `shouldReturn` 0

  describe "Revealed in Wrath" do
    it "blanks a non-unique enemy's text box for the phase" . withHeroes [glorfindelCode, eowynCode, legolasCode] $ \pid -> do
      eid <- staged Enemies.forestSpider
      wrath <- putCardIntoHand pid Events.revealedInWrath
      giveResources legolasCode 1
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget wrath.id)
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem Blank)

    it "needs a Noldor hero" . withHeroes [beravorCode, eowynCode, legolasCode] $ \pid -> do
      _ <- staged Enemies.forestSpider
      wrath <- putCardIntoHand pid Events.revealedInWrath
      giveResources legolasCode 1
      run $ ActionWindow pid
      offersCard pid wrath `shouldReturn` False

  describe "Rhovanion Outrider" do
    it "places 1 progress on a staged location and gets +1 willpower" . gameTest $ \pid -> do
      outrider <- putAllyIntoPlay pid Allies.rhovanionOutrider
      lid <- stagedLocation Locations.mountainsOfMirkwood
      run $ CommitToQuest outrider
      run $ CheckWindows [mkAfter $ CommittedToQuest outrider]
      chooseAbilityOf pid outrider
      (.progress) <$> getLocation lid `shouldReturn` 1
      getWillpower outrider `shouldReturn` 2

    it "gets no willpower when the progress explores the location" . gameTest $ \pid -> do
      outrider <- putAllyIntoPlay pid Allies.rhovanionOutrider
      lid <- stagedLocation Locations.oldForestRoad
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
      run $ CommitToQuest outrider
      run $ CheckWindows [mkAfter $ CommittedToQuest outrider]
      chooseAbilityOf pid outrider
      getsGame (length . (.entities.locations)) `shouldReturn` 0
      getWillpower outrider `shouldReturn` 1

  describe "Scouting Party" do
    it "gives each committed character +2 willpower when all are Scouts" . gameTest $ \pid -> do
      outrider <- putAllyIntoPlay pid Allies.rhovanionOutrider
      party <- putCardIntoHand pid Events.scoutingParty
      giveResources eowynCode 2
      run $ CommitToQuest outrider
      run $ CheckWindows [mkAfter $ CommittedToQuest outrider]
      chooseTarget pid (CardIdTarget party.id)
      getWillpower outrider `shouldReturn` 3

    it "is not offered when a committed character is not a Scout" . gameTest $ \pid -> do
      outrider <- putAllyIntoPlay pid Allies.rhovanionOutrider
      spearman <- putAllyIntoPlay pid Allies.gondorianSpearman
      party <- putCardIntoHand pid Events.scoutingParty
      giveResources eowynCode 2
      run $ CommitToQuest outrider
      run $ CommitToQuest spearman
      run $ CheckWindows [mkAfter $ CommittedToQuest outrider, mkAfter $ CommittedToQuest spearman]
      offersCard pid party `shouldReturn` False

  describe "Entangling Nets" do
    it "attaches to the next enemy entering the staging area, giving it -2 attack and -2 defense" . gameTest $ \pid -> do
      _ <- playWith pid beravorCode 1 Attachments.entanglingNets
      passActions pid
      eid <- staged Enemies.forestSpider
      getEnemyAttack eid `shouldReturn` 0
      getEnemyDefense eid `shouldReturn` 0

  describe "Arrows from the Trees" do
    it "deals 1 damage to each staged enemy, and 2 more for 3 tactics resources" . withHeroes [beravorCode, eowynCode, legolasCode] $ \pid -> do
      eid <- staged Enemies.forestSpider
      arrows <- putCardIntoHand pid Events.arrowsFromTheTrees
      giveResources beravorCode 1
      giveResources legolasCode 3
      afterEngagementChecks
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget arrows.id)
      chooseLabel "$label.arrowsFromTheTrees.damage" pid
      legolas <- hero legolasCode
      replicateM_ 3 $ chooseTarget pid legolas
      getDamage (EnemyTarget eid) `shouldReturn` 3

    it "deals nothing once an enemy was engaged this phase" . gameTest $ \pid -> do
      eid <- staged Enemies.forestSpider
      arrows <- putCardIntoHand pid Events.arrowsFromTheTrees
      giveResources beravorCode 1
      afterEngagementChecks
      run $ CreateEffect $ Effect GameSource GameTarget [EnemyEngagedThisPhase] UntilEndOfPhase
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget arrows.id)
      getDamage (EnemyTarget eid) `shouldReturn` 0

    it "is not played before the engagement checks" . gameTest $ \pid -> do
      _ <- staged Enemies.forestSpider
      arrows <- putCardIntoHand pid Events.arrowsFromTheTrees
      giveResources beravorCode 1
      putGame . (phaseL .~ EncounterPhase) =<< getGame
      run $ ActionWindow pid
      offersCard pid arrows `shouldReturn` False
