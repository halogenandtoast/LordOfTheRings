-- | Player cards whose text reaches past the common case.
module LordOfTheRings.PlayerCardTextSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (..), CampaignLogKey (Determined), newCampaignLog, recorded)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Message.Lifted (lowerThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theWastesOfEriador, voyageAcrossBelegaer)
import LordOfTheRings.Trait (Trait (Dwarf))
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

progressOf :: Target -> GameT Int
progressOf t = getsGame (maybe 0 (.progress) . find ((== t) . toTarget) . toList . (.entities.quests))

allySource :: Target -> Source
allySource = \case
  AllyTarget aid -> AllySource aid
  t -> error $ "not an ally: " <> show t

attachmentSource :: Target -> Source
attachmentSource = \case
  AttachmentTarget aid -> AttachmentSource aid
  t -> error $ "not an attachment: " <> show t

allyOf :: CardDef -> GameT Target
allyOf def = getsGame (toTarget . fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

-- | An objective-ally put into play under the player, as a ship of the fleet.
objectiveAlly :: PlayerId -> CardDef -> GameT Target
objectiveAlly pid def = do
  card <- genEncounterCard def
  run $ PutObjectiveAllyIntoPlayUnder pid card
  allyOf def

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

putInDiscard :: PlayerId -> CardDef -> GameT Card
putInDiscard pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  putGame . (playersL . ix pid . discardL %~ (card :)) =<< getGame
  pure card

attachmentsOn :: Target -> GameT [CardCode]
attachmentsOn host = getsGame (map toCardCode . filter ((== AttachedTo host) . attachmentPlacement . toAttrs) . toList . (.entities.attachments))

-- | Leave the open questions unanswered, as the planning window, to run what comes next.
dropQuestions :: GameT ()
dropQuestions = putGame . (questionL .~ mempty) =<< getGame

beravor :: GameT Target
beravor = toTarget <$> getHeroWithCode "01012"

theodred :: GameT Target
theodred = toTarget <$> getHeroWithCode "01002"

spec :: Spec
spec = describe "Player card text" do
  describe "Berelind" do
    it "places its progress on a quest card of the player's choice, a side quest among them" . gameTest $ \pid -> do
      main <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Quests.scavengeForSupplies
      side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== toCardCode Quests.scavengeForSupplies) . toCardCode) . toList . (.entities.quests))
      berelind <- putAllyIntoPlay pid Allies.berelind
      run $ UseCardAbility pid (allySource berelind) 1 []
      chooseTarget pid side
      progressOf side `shouldReturn` 1
      progressOf main `shouldReturn` 0

  describe "Calaerion" do
    it "upgraded, shuffles a card discarded from the encounter deck back into it" . gameTestIn voyageAcrossBelegaer testDeck $ \pid -> do
      calaerion <- objectiveAlly pid Objectives.calaerionUpgraded
      [top, _] <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DiscardTopOfEncounterDeck GameSource GameTarget 1
      chooseAbilityOf pid calaerion
      map (.id) <$> getEncounterDiscard `shouldReturn` []
      map (.id) <$> getEncounterDeck >>= (`shouldSatisfy` elem top.id)

  describe "Captive Villager" do
    it "counts its +5 threat in an ambush's engagement check" . gameTest $ \pid -> do
      -- 23 threat, short of the Taskmaster's 27 but for the Villager's 5
      run $ RaiseThreat pid 23
      _ <- objectiveAlly pid Objectives.captiveVillager
      taskmaster <- stagedEnemy Enemies.goblinTaskmaster
      getsGame (fmap (.placement) . find ((== taskmaster) . toId) . toList . (.entities.enemies)) `shouldReturn` Just (EngagedWith pid)

  describe "Curious Brandybuck" do
    it "goes to the bottom of its owner's deck after the active location is explored" . gameTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.curiousBrandybuck
      road <- stagedLocation Locations.oldForestRoad
      run $ PlaceLocation road ActiveLocation
      run $ PlaceTokens GameSource (toTarget road) Progress 3
      map toCardCode . lastMay . (.deck) <$> getPlayer pid `shouldReturn` Just (toCardCode Allies.curiousBrandybuck)

  describe "Master of Lore" do
    it "as printed, lowers a lore card's cost to no less than 1" . gameTest $ \pid -> do
      withoutErrata pid
      master <- putAllyIntoPlay pid Allies.masterOfLore
      run $ UseCardAbility pid (allySource master) 1 []
      chooseLabel "$label.masterOfLore.ally" pid
      henamarth <- putCardIntoHand pid Allies.henamarthRiversong
      gleowine <- putCardIntoHand pid Allies.gleowine
      costModifier pid henamarth `shouldReturn` 0
      costModifier pid gleowine `shouldReturn` (-1)

  describe "Starla" do
    it "is played from the discard pile after she is discarded from hand" . gameTest $ \pid -> do
      starla <- putCardIntoHand pid Allies.starla
      run $ DiscardFromHand pid starla.id
      chooseTarget pid (CardIdTarget starla.id)
      getsGame (any ((== starla.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True

  describe "Chieftain's Brooch" do
    it "reads a trait its hero gained" . gameTest $ \pid -> do
      b <- beravor
      run $ CreateEffect $ Effect GameSource b [GainTrait Dwarf] UntilEndOfPhase
      slayer <- putAllyIntoPlay pid Allies.longbeardOrcSlayer
      _ <- putAttachmentOn pid Attachments.chieftainsBrooch b
      getModifiers slayer >>= (`shouldSatisfy` elem (WillpowerModifier 1))

  describe "Enlarged Quarters" do
    it "upgraded, makes its Ship count as 1 more character in a Sailing test" . gameTestIn voyageAcrossBelegaer testDeck $ \pid -> do
      silverWing <- objectiveAlly pid Objectives.silverWing
      _ <- putAttachmentOn pid Attachments.enlargedQuartersUpgraded silverWing
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run SailingTest
      chooseTarget pid silverWing
      chooseLabel "$label.sailingTest.done" pid
      chooseLabel "$label.continue" pid
      length <$> getEncounterDiscard `shouldReturn` 2

  describe "Mail of Earnur" do
    it "answers a threat reduced by any effect" . gameTest $ \pid -> do
      b <- beravor
      mail <- putAttachmentOn pid Attachments.mailOfEarnur b
      run $ RaiseThreat pid 10
      runQueueT (lowerThreat pid 2)
      _ <- mainLoop
      offersAbilityOf pid mail `shouldReturn` True

  describe "Ruthless" do
    it "moves the amount of damage chosen" . gameTest $ \pid -> do
      b <- beravor
      ruthless <- putAttachmentOn pid Attachments.ruthless b
      run $ PlaceTokens GameSource b Damage 3
      troll <- stagedEnemy Enemies.hillTroll
      run $ EngageEnemy pid troll
      run $ UseCardAbility pid (attachmentSource ruthless) 1 []
      chooseLabel "$label.amount n=i:2" pid
      getDamage b `shouldReturn` 1
      getDamage (EnemyTarget troll) `shouldReturn` 2

  describe "Silverthorn" do
    it "adds the enemy its hero destroyed to the victory display" . gameTest $ \pid -> do
      b <- beravor
      silverthorn <- putAttachmentOn pid Attachments.silverthorn b
      destroyed <- genEncounterCard Enemies.kingSpider
      other <- genEncounterCard Enemies.hillTroll
      run $ AddToEncounterDiscard [destroyed]
      run $ AddToEncounterDiscard [other]
      eid <- EnemyId <$> getRandom
      run $ UseCardAbility pid (attachmentSource silverthorn) 1 [mkAfter $ EnemyDestroyedByAttack eid [b] mempty, mkAfter $ EnemyCardDestroyedByAttack eid destroyed.id]
      getsGame (map (.id) . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [destroyed.id]

  describe "Stalwart" do
    it "plays an attachment from the discard pile on its hero, paying for it" . gameTest $ \pid -> do
      t <- theodred
      stalwart <- putAttachmentOn pid Attachments.stalwart t
      steward <- putInDiscard pid Attachments.stewardOfGondor
      run $ PlaceTokens GameSource t Resource 2
      run $ UseCardAbility pid (attachmentSource stalwart) 1 []
      chooseTarget pid (CardIdTarget steward.id)
      attachmentsOn t >>= (`shouldSatisfy` elem (toCardCode Attachments.stewardOfGondor))
      getDamage t `shouldReturn` 0
      (.resources) <$> getHeroWithCode "01002" `shouldReturn` 0

  describe "Throwing Axe" do
    it "upgraded, may be tossed onto no enemy" . gameTest $ \pid -> do
      b <- beravor
      axe <- putAttachmentOn pid Attachments.throwingAxeUpgraded b
      troll <- stagedEnemy Enemies.hillTroll
      run $ UseCardAbility pid (attachmentSource axe) 1 []
      chooseLabel "$label.throwingAxe.toss" pid
      chooseLabel "$label.throwingAxe.missed" pid
      getDamage (EnemyTarget troll) `shouldReturn` 0

  describe "Come Together" do
    it "gives a character the Trait of the player's choice with a Lore hero" . gameTest $ \pid -> do
      _ <- playWith pid "01012" 1 Events.comeTogether
      b <- beravor
      chooseTarget pid b
      chooseLabel "$label.comeTogether.gainTrait trait=s:\"Dwarf\"" pid
      getModifiers b >>= (`shouldSatisfy` elem (GainTrait Dwarf))

  describe "Command" do
    it "Leadership: the ally put into play is discarded at the end of the phase" . gameTest $ \pid -> do
      henamarth <- putInDiscard pid Allies.henamarthRiversong
      _ <- playWith pid "01002" 1 Events.commandLeadership
      chooseLabel "$label.command.allyFromDiscard" pid
      ally <- allyOf Allies.henamarthRiversong
      dropQuestions
      getModifiers (CardIdTarget henamarth.id) >>= (`shouldSatisfy` elem DiscardAtEndOfPhase)
      run $ EndPhase CombatPhase
      getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
      map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem henamarth.id)

    it "Tactics: the readied attachment may move to another character" . gameTest $ \pid -> do
      b <- beravor
      t <- theodred
      axe <- putAttachmentOn pid Attachments.throwingAxe b
      run $ Exhaust axe
      command <- putCardIntoHand pid Events.commandTactics
      for_ [pc | PlayerCard pc <- [command]] \pc -> run $ PlayEvent pid pc []
      chooseLabel "$label.command.readyAttachment" pid
      chooseTarget pid t
      attachmentsOn t >>= (`shouldSatisfy` elem (toCardCode Attachments.throwingAxe))

  describe "Determination" do
    let angmar = (newCampaignLog "angmar-awakened") {campaignLogHeroes = [["01012", "01007", "01002"]]}
    it "stays attached, and marks its character determined if the scenario is won" . campaignTestIn theWastesOfEriador angmar testDeck $ \pid -> do
      card <- playWith pid "01012" 1 Events.determination
      b <- beravor
      chooseTarget pid b
      attachmentsOn b >>= (`shouldSatisfy` elem (toCardCode Events.determination))
      map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` notElem card.id)
      recorded Determined <$> getCampaignLog `shouldReturn` []
      dropQuestions
      run $ GameOver Won
      recorded Determined <$> getCampaignLog >>= (`shouldSatisfy` elem "01012")

  describe "Galdor's Counsel" do
    it "Lore: puts the cards not taken on the top or bottom of the deck, as the player chooses" . gameTest $ \pid -> do
      top <- headMay . (.deck) <$> getPlayer pid
      _ <- playWith pid "01012" 1 Events.galdorsCounselLore
      chooseLabel "$label.galdorsCounsel.top" pid
      chooseLabel "$label.galdorsCounsel.done" pid
      chooseLabel "$label.galdorsCounsel.onBottom" pid
      map (.id) . lastMay . (.deck) <$> getPlayer pid `shouldReturn` map (.id) top

  describe "Lore of Ancient Arnor" do
    it "returns two different cards when the player plays alone" . gameTest $ \pid -> do
      a <- putInDiscard pid Allies.henamarthRiversong
      b <- putInDiscard pid Allies.gleowine
      _ <- putInDiscard pid Allies.longbeardOrcSlayer
      _ <- playWith pid "01012" 3 Events.loreOfAncientArnor
      chooseTarget pid (CardIdTarget a.id)
      offered <- maybe [] questionChoices <$> getQuestion pid
      offered `shouldSatisfy` notElem (CardIdTarget a.id) . mapMaybe (\case TargetLabel t _ -> Just t; _ -> Nothing)
      chooseTarget pid (CardIdTarget b.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` \h -> a.id `elem` h && b.id `elem` h)

  describe "The Houses of Healing" do
    it "cannot be played without a hero in a discard pile" . gameTest $ \pid -> do
      card <- putCardIntoHand pid Events.theHousesOfHealing
      putGame . (phaseL .~ RefreshPhase) =<< getGame
      let houses = case card of
            PlayerCard pc -> pc
            _ -> error "a player card"
      actionEventPlayable houses `shouldReturn` False
      _ <- putInDiscard pid Allies.henamarthRiversong
      actionEventPlayable houses `shouldReturn` False
      _ <- putInDiscard pid Heroes.gimli
      actionEventPlayable houses `shouldReturn` True

  describe "Tides of Fate" do
    it "gives +3 attack for the character's next attack only" . gameTest $ \_ -> do
      b <- beravor
      troll <- stagedEnemy Enemies.hillTroll
      run $ CreateEffect $ Effect GameSource b [ForNextAttack (AttackModifier 3)] UntilEndOfPhase
      getModifiers b >>= (`shouldSatisfy` notElem (AttackModifier 3))
      run $ CheckWindows [mkAfter $ DeclaredAsAttacker b troll]
      getModifiers b >>= (`shouldSatisfy` elem (AttackModifier 3))
      run $ AttackResolved troll Nothing [b] mempty
      getModifiers b >>= (`shouldSatisfy` notElem (AttackModifier 3))
