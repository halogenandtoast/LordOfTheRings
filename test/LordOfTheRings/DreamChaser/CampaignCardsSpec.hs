module LordOfTheRings.DreamChaser.CampaignCardsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Scenario (raidOnTheGreyHavens, voyageAcrossBelegaer)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

dc :: CampaignLog
dc = (newCampaignLog "dream-chaser") {campaignLogHeroes = [["01001", "01002", "01003"]]}

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

victoryCodes :: GameT [CardCode]
victoryCodes = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

spec :: Spec
spec = describe "Dream-chaser campaign cards" do
  describe "Ominous Fate" do
    it "raises threat by the campaign card's part number, and may go to the victory display" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      threat <- (.threat) <$> getPlayer pid
      run . RevealCard =<< genEncounterCard Treacheries.ominousFate
      resolveRevealed
      chooseOptionMatching "raise threat" pid \case
        Label l _ -> "$label.ominousFate.threat" `isPrefixOf` l
        _ -> False
      (.threat) <$> getPlayer pid `shouldReturn` threat + 1
      chooseLabel "$label.ominousFate.addToVictory" pid
      victoryCodes `shouldReturn` [toCardCode Treacheries.ominousFate]

    it "adds 1 to X for each boon chosen during setup" . campaignTestIn raidOnTheGreyHavens dc {campaignLogNotes = mapFromList [(ChosenDuringSetup, [toCardCode Events.galdorsCounselTactics])]} starterDeck $ \pid -> do
      threat <- (.threat) <$> getPlayer pid
      run . RevealCard =<< genEncounterCard Treacheries.ominousFate
      resolveRevealed
      chooseOptionMatching "raise threat" pid \case
        Label l _ -> "$label.ominousFate.threat" `isPrefixOf` l
        _ -> False
      -- Raid on the Grey Havens is part 3
      (.threat) <$> getPlayer pid `shouldReturn` threat + 4

  describe "Crowley" do
    it "takes a resource as he engages, growing with it" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      crowley <- staged Enemies.crowley
      run $ EngageEnemy pid crowley
      e <- getEnemy crowley
      countTokens Resource (enemyTokens $ toAttrs e) `shouldBe` 1
      mods <- getModifiers crowley
      mods `shouldSatisfy` elem (AttackModifier 1)

  describe "Revenant" do
    it "takes no more than 2 damage at once, and grows with the campaign card's resources" . campaignTestIn voyageAcrossBelegaer dc {campaignLogCardResources = 1} starterDeck $ \_ -> do
      revenant <- staged Enemies.revenant
      run $ DealDamage GameSource (toTarget revenant) 3
      (.damage) <$> getEnemy revenant `shouldReturn` 2
      mods <- getModifiers revenant
      mods `shouldSatisfy` elem (AttackModifier 2)

  describe "Thunderstruck" do
    it "takes no more than 4 damage each round" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \_ -> do
      ship <- staged Enemies.thunderstruck
      run $ PlaceTokens GameSource (toTarget ship) Damage 3
      run $ PlaceTokens GameSource (toTarget ship) Damage 3
      (.damage) <$> getEnemy ship `shouldReturn` 4

  describe "Black Key (burden)" do
    it "gives the enemy it is attached to +1 threat, attack and defense" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \_ -> do
      crowley <- staged Enemies.crowley
      run . (`PutObjectiveIntoPlayAttached` toTarget crowley) =<< genEncounterCard Objectives.blackKeyBurden
      mods <- getModifiers crowley
      mods `shouldSatisfy` \ms -> all (`elem` ms) [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, ImmuneToPlayerCardEffects]

  describe "Galdor's Counsel" do
    it "adds half its level in resources, then goes to the victory display" . campaignTestIn voyageAcrossBelegaer dc {campaignLogMarked = mapFromList [(toCardCode Events.galdorsCounselLeadership, 3)]} starterDeck $ \pid -> do
      _ <- playWith pid "01001" 0 Events.galdorsCounselLeadership
      aragorn <- getHeroWithCode "01001"
      chooseTarget pid aragorn.id
      getHeroResources aragorn.id `shouldReturn` 2
      victoryCodes `shouldReturn` [toCardCode Events.galdorsCounselLeadership]

  describe "Parrying Cutlass" do
    it "gives +1 defense, +2 with 2 resources in the pool" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      gloin <- getHeroWithCode "01003"
      _ <- putAttachmentOn pid Attachments.parryingCutlass (toTarget gloin.id)
      (`shouldSatisfy` elem (DefenseModifier 1)) =<< getModifiers gloin.id
      run $ PlaceTokens GameSource (toTarget gloin.id) Resource 2
      (`shouldSatisfy` elem (DefenseModifier 2)) =<< getModifiers gloin.id

  describe "Send for Help" do
    let reveal def = do
          _ <- setEncounterDeck []
          run . RevealCard =<< genEncounterCard def
          resolveRevealed
        upgraded def = dc {campaignLogMarked = mapFromList [(toCardCode def, 4)]}
        alliesIn = getsGame (map toCardCode . toList . (.entities.allies))
    it "plays a card from the top 10 of the deck at no cost (Leadership)" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      archer <- PlayerCard <$> genPlayerCard Allies.silverlodeArcher (Just pid)
      run $ PutOnTopOfDeck pid archer
      resources <- sum . map (.resources) . toList <$> getsGame (.entities.heroes)
      reveal Treacheries.sendForHelpLeadership
      chooseTarget pid (CardIdTarget archer.id)
      chooseLabel "$label.sendForHelp.play" pid
      alliesIn `shouldReturn` [toCardCode Allies.silverlodeArcher]
      (sum . map (.resources) . toList <$> getsGame (.entities.heroes)) `shouldReturn` resources
      ((.deck) <$> getPlayer pid) >>= (`shouldSatisfy` notElem archer.id . map (.id))

    it "puts a hero from the collection into play, removed from the game as it leaves play (Tactics)" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      reveal Treacheries.sendForHelpTactics
      chooseOptionMatching "choose a hero" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      hero <- getsGame (fromJustNote "no hero from the collection" . find ((`notElem` ["01001", "01002", "01003"]) . toCardCode) . toList . (.entities.heroes))
      run $ Discard (toTarget hero)
      getsGame (Map.member (toId hero) . (.entities.heroes)) `shouldReturn` False
      ((.discard) <$> getPlayer pid) >>= (`shouldSatisfy` notElem (toCardCode hero) . map toCardCode)

    it "removes the hero from the game at the end of the round (Tactics)" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      heroCount <- getsGame (length . (.entities.heroes))
      reveal Treacheries.sendForHelpTactics
      chooseOptionMatching "choose a hero" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      getsGame (length . (.entities.heroes)) `shouldReturn` heroCount + 1
      run EndRound
      getsGame (length . (.entities.heroes)) `shouldReturn` heroCount
      ((.discard) <$> getPlayer pid) `shouldReturn` []

    it "adds an instance of the chosen phase after the current one (Spirit)" . campaignTestIn voyageAcrossBelegaer dc starterDeck $ \pid -> do
      reveal Treacheries.sendForHelpSpirit
      chooseLabel "$label.sendForHelp.resource" pid
      resources <- sum . map (.resources) . toList <$> getsGame (.entities.heroes)
      hand <- length . (.hand) <$> getPlayer pid
      -- the resource phase asks nothing, and the round goes on to its travel phase
      run $ EndPhase QuestPhase
      (sum . map (.resources) . toList <$> getsGame (.entities.heroes)) `shouldReturn` resources + 3
      length . (.hand) <$> getPlayer pid `shouldReturn` hand + 1
      getsGame (.phase) `shouldReturn` TravelPhase

    it "adds two phases, in the order chosen, when upgraded (Spirit)" . campaignTestIn voyageAcrossBelegaer (upgraded Treacheries.sendForHelpSpirit) starterDeck $ \pid -> do
      reveal Treacheries.sendForHelpSpirit
      chooseLabel "$label.sendForHelp.planning" pid
      chooseOptionMatching "no second planning" pid \case
        Label "$label.sendForHelp.planning" _ -> error "planning offered twice"
        Label "$label.sendForHelp.resource" _ -> True
        _ -> False
      resources <- sum . map (.resources) . toList <$> getsGame (.entities.heroes)
      run $ EndPhase QuestPhase
      getsGame (.phase) `shouldReturn` PlanningPhase
      (sum . map (.resources) . toList <$> getsGame (.entities.heroes)) `shouldReturn` resources
      chooseOptionMatching "end the planning phase" pid \case
        Label l _ -> l `elem` ["$label.pass", "$label.donePlanning"] || "$label.proceedTo." `isPrefixOf` l
        _ -> False
      (sum . map (.resources) . toList <$> getsGame (.entities.heroes)) `shouldReturn` resources + 3
      getsGame (.phase) `shouldReturn` TravelPhase

    it "lets Gandalf choose up to 3 of his options when upgraded (Lore)" . campaignTestIn voyageAcrossBelegaer (upgraded Treacheries.sendForHelpLore) starterDeck $ \pid -> do
      run $ RaiseThreat pid 10
      threat <- (.threat) <$> getPlayer pid
      hand <- length . (.hand) <$> getPlayer pid
      reveal Treacheries.sendForHelpLore
      chooseAbility pid
      chooseLabel "$label.gandalf.draw" pid
      chooseLabel "$label.gandalf.threat" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` hand + 3
      (.threat) <$> getPlayer pid `shouldReturn` threat - 5

  describe "Upgraded sides" do
    it "has each upgradable card's Upgraded side among the cards" $ (id :: IO () -> IO ()) $
      for_ ["DCC172U", "DCC173U", "DCC174U", "DCC175U", "DCC180U", "DCC183U", "DCC189U", "DCC190U", "DCC192U", "DCC193U", "DCC136U", "DCC137U", "DCC140U", "DCC141U", "DCC078U"] \code ->
        toCardCode (toCardDef (code :: CardCode)) `shouldBe` code
