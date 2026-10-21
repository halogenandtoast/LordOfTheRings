module LordOfTheRings.Starter.DwarvesOfDurinSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally (Ally)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (PlayerCard (..))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (locationPlacement)
import LordOfTheRings.Modifier (ModifierType (GainResourceIcon))
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Sphere (Sphere (Spirit))
import LordOfTheRings.Window (WindowType (CommittedToQuest), mkAfter)
import TestImport

-- Dáin Ironfoot (Leadership), Ori and Bifur (Lore).
dainCode, oriCode, bifurCode :: CardCode
dainCode = "02116"
oriCode = "131004"
bifurCode = "03002"

dodTest :: (PlayerId -> GameT ()) -> IO ()
dodTest = gameTestWith (coreDeck "dwarves-of-durin")

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

resources :: CardCode -> GameT Int
resources code = (.resources) <$> getHeroWithCode code

-- | Replace the player's deck, top card first.
setDeck :: PlayerId -> [CardDef] -> GameT [PlayerCard]
setDeck pid defs = do
  cards <- traverse (`genPlayerCard` Just pid) defs
  g <- getGame
  putGame $ g & playersL . ix pid . deckL .~ map PlayerCard cards
  pure cards

deckIds, discardIds, handIds :: PlayerId -> GameT [CardId]
deckIds pid = map (.id) . (.deck) <$> getPlayer pid
discardIds pid = map (.id) . (.discard) <$> getPlayer pid
handIds pid = map (.id) . (.hand) <$> getPlayer pid

allyCodes :: GameT [CardCode]
allyCodes = getsGame (sort . map toCardCode . toList . (.entities.allies))

allyWithCode :: CardDef -> GameT Ally
allyWithCode def = getsGame (fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

planning :: GameT ()
planning = run $ BeginPhase PlanningPhase

spec :: Spec
spec = describe "Dwarves of Durin" do
  describe "Dáin Ironfoot" do
    it "gives Dwarf characters +1 willpower while he is ready" . dodTest $ \_ -> do
      ori <- hero oriCode
      run $ Run []
      getWillpower ori `shouldReturn` 3
      run . Exhaust =<< hero dainCode
      getWillpower ori `shouldReturn` 2

  describe "Bifur" do
    it "takes 1 resource from another hero's pool" . dodTest $ \pid -> do
      ori <- hero oriCode
      run $ PlaceTokens GameSource ori Resource 1
      planning
      chooseAbilityOf pid =<< hero bifurCode
      resources bifurCode `shouldReturn` 1
      resources oriCode `shouldReturn` 0

  describe "Nori" do
    it "lowers your threat by 1 after you play a Dwarf ally from hand" . dodTest $ \pid -> do
      run . PutHeroIntoPlay pid =<< genPlayerCard Heroes.nori (Just pid)
      run $ RaiseThreat pid 10
      _ <- playWith pid oriCode 1 Allies.ereborRecordKeeper
      chooseAbilityOf pid =<< hero (toCardCode Heroes.nori)
      (.threat) <$> getPlayer pid `shouldReturn` 9

  describe "Ori" do
    it "draws 1 more card in the resource phase with 5 Dwarf characters" . dodTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.zigilMiner
      _ <- putAllyIntoPlay pid Allies.zigilMiner
      before <- length <$> handIds pid
      run $ BeginPhase ResourcePhase
      length <$> handIds pid `shouldReturn` (before + 2)

  describe "Zigil Miner" do
    it "adds a resource for each discarded card that costs the named number" . dodTest $ \pid -> do
      miner <- putAllyIntoPlay pid Allies.zigilMiner
      _ <- setDeck pid [Allies.ereborRecordKeeper, Attachments.legacyOfDurin, Allies.bombur]
      planning
      chooseAbilityOf pid miner
      chooseOptionMatching "name 1" pid \case
        Label l _ -> l == "$label.zigilMiner.number number=i:1"
        _ -> False
      chooseTarget pid =<< hero oriCode
      resources oriCode `shouldReturn` 2
      length <$> discardIds pid `shouldReturn` 2

  describe "Erebor Record Keeper" do
    it "pays 1 Lore resource to ready a Dwarf character" . dodTest $ \pid -> do
      keeper <- putAllyIntoPlay pid Allies.ereborRecordKeeper
      dain <- hero dainCode
      run $ Exhaust dain
      run . (\o -> PlaceTokens GameSource o Resource 1) =<< hero oriCode
      planning
      chooseAbilityOf pid keeper
      chooseTarget pid dain
      heroExhausted . toAttrs <$> getHeroWithCode dainCode `shouldReturn` False
      resources oriCode `shouldReturn` 0

  describe "Lure of Moria" do
    it "readies all Dwarf characters" . dodTest $ \pid -> do
      for_ [dainCode, oriCode] \c -> run . Exhaust =<< hero c
      _ <- playWith pid dainCode 3 Events.lureOfMoria
      map (heroExhausted . toAttrs) <$> traverse getHeroWithCode [dainCode, oriCode] `shouldReturn` [False, False]

  describe "Bombur" do
    it "gives a location -1 threat" . dodTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.mountainsOfMirkwood
      before <- getStagingThreat
      bombur <- putAllyIntoPlay pid Allies.bombur
      planning
      chooseAbilityOf pid bombur
      chooseTarget pid =<< getsGame (toTarget . fromJustNote "no location" . headMay . toList . (.entities.locations))
      getStagingThreat `shouldReturn` (before - 1)

  describe "Longbeard Elder" do
    it "places 1 progress on the quest when the top encounter card is a location" . dodTest $ \pid -> do
      _ <- putQuestStage Quests.fliesAndSpiders
      _ <- setEncounterDeck [Locations.mountainsOfMirkwood]
      elder <- putAllyIntoPlay pid Allies.longbeardElder
      run $ CheckWindows [mkAfter $ CommittedToQuest elder]
      chooseAbilityOf pid elder
      chooseLabel "$label.continue" pid
      (.progress) <$> getActiveQuest `shouldReturn` 1

  describe "Ered Nimrais Prospector" do
    it "discards the top 3 cards, then shuffles 1 from the discard pile back" . dodTest $ \pid -> do
      cards <- setDeck pid [Attachments.cram, Attachments.cram, Attachments.cram]
      prospector <- putAllyIntoPlay pid Allies.eredNimraisProspector
      chooseAbilityOf pid prospector
      chooseTarget pid (CardIdTarget (fromJustNote "no card" (headMay cards)).id)
      length <$> discardIds pid `shouldReturn` 2
      length <$> deckIds pid `shouldReturn` 1

  describe "Hidden Cache" do
    it "adds 2 resources to a hero after it is discarded from your deck" . dodTest $ \pid -> do
      cache <- setDeck pid [Events.hiddenCache, Attachments.cram]
      run $ DiscardTopOfDeck pid 1
      chooseTarget pid (CardIdTarget (fromJustNote "no card" (headMay cache)).id)
      chooseTarget pid =<< hero oriCode
      resources oriCode `shouldReturn` 2

    it "spends 1 resource to draw 1 card" . dodTest $ \pid -> do
      deck <- length <$> deckIds pid
      _ <- playWith pid oriCode 1 Events.hiddenCache
      resources oriCode `shouldReturn` 0
      length <$> deckIds pid `shouldReturn` (deck - 1)

  describe "Ered Luin Miner" do
    it "goes into play after it is discarded from your deck" . dodTest $ \pid -> do
      miner <- setDeck pid [Allies.eredLuinMiner]
      run $ DiscardTopOfDeck pid 1
      chooseTarget pid (CardIdTarget (fromJustNote "no card" (headMay miner)).id)
      allyCodes `shouldReturn` [toCardCode Allies.eredLuinMiner]
      discardIds pid `shouldReturn` []

  describe "Fili" do
    it "puts Kili into play from your deck after you play him in the planning phase" . dodTest $ \pid -> do
      deck <- setDeck pid [Attachments.cram, Allies.kili]
      _ <- playWith pid dainCode 3 Allies.fili
      chooseAbilityOf pid =<< allyWithCode Allies.fili
      for_ (find ((== toCardCode Allies.kili) . toCardCode) deck) \kili -> chooseTarget pid (CardIdTarget kili.id)
      allyCodes `shouldReturn` sort [toCardCode Allies.fili, toCardCode Allies.kili]
      length <$> deckIds pid `shouldReturn` 1

  describe "Dori" do
    it "takes the damage a hero was dealt" . dodTest $ \pid -> do
      dori <- putAllyIntoPlay pid Allies.dori
      dain <- hero dainCode
      run $ DealDamage GameSource dain 2
      chooseAbilityOf pid dori
      getDamage dain `shouldReturn` 0
      getDamage dori `shouldReturn` 2

  describe "Glóin" do
    it "adds 2 resources to a hero after you play him with 5 Dwarf characters" . dodTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.zigilMiner
      _ <- playWith pid dainCode 3 Allies.gloin
      chooseAbilityOf pid =<< allyWithCode Allies.gloin
      chooseTarget pid =<< hero oriCode
      resources oriCode `shouldReturn` 2

  describe "Erebor Guard" do
    it "costs 2 less to play, discarding the top 2 cards of your deck" . dodTest $ \pid -> do
      dain <- hero dainCode
      run $ CreateEffect $ Effect GameSource dain [GainResourceIcon Spirit] UntilEndOfRound
      deck <- length <$> deckIds pid
      _ <- playWith pid dainCode 2 Allies.ereborGuard
      allyCodes `shouldReturn` [toCardCode Allies.ereborGuard]
      resources dainCode `shouldReturn` 0
      length <$> deckIds pid `shouldReturn` (deck - 2)

  describe "Narvi's Belt" do
    it "gives the attached hero a resource icon" . dodTest $ \pid -> do
      dain <- hero dainCode
      belt <- putAttachmentOn pid Attachments.narvisBelt dain
      run $ PlaceTokens GameSource dain Resource 1
      planning
      chooseAbilityOf pid belt
      chooseLabel "$label.narvisBelt.spirit" pid
      (`shouldReturn` 1) . resourcesInSphere Spirit =<< getPlayer pid

  describe "Legacy of Durin" do
    it "draws 1 card after you play a Dwarf ally from hand" . dodTest $ \pid -> do
      legacy <- putAttachmentOn pid Attachments.legacyOfDurin =<< hero dainCode
      before <- length <$> handIds pid
      _ <- playWith pid oriCode 1 Allies.ereborRecordKeeper
      chooseAbilityOf pid legacy
      length <$> handIds pid `shouldReturn` (before + 1)

  describe "Healing Herbs" do
    it "exhausts the attached hero to heal all damage on a character" . dodTest $ \pid -> do
      ori <- hero oriCode
      dain <- hero dainCode
      herbs <- putAttachmentOn pid Attachments.healingHerbs ori
      run $ PlaceTokens GameSource dain Damage 2
      planning
      chooseAbilityOf pid herbs
      chooseTarget pid dain
      getDamage dain `shouldReturn` 0
      heroExhausted . toAttrs <$> getHeroWithCode oriCode `shouldReturn` True
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "We Are Not Idle" do
    it "exhausts X Dwarf heroes to add X resources to a hero and draw 1 card" . dodTest $ \pid -> do
      before <- length <$> handIds pid
      _ <- playWith pid dainCode 0 Events.weAreNotIdle
      chooseTarget pid =<< hero oriCode
      chooseTarget pid =<< hero bifurCode
      chooseLabel "$label.weAreNotIdle.done" pid
      chooseTarget pid =<< hero dainCode
      resources dainCode `shouldReturn` 2
      length <$> handIds pid `shouldReturn` (before + 1)

  describe "Cram" do
    it "is discarded to ready the attached hero" . dodTest $ \pid -> do
      dain <- hero dainCode
      cram <- putAttachmentOn pid Attachments.cram dain
      run $ Exhaust dain
      planning
      chooseAbilityOf pid cram
      heroExhausted . toAttrs <$> getHeroWithCode dainCode `shouldReturn` False

  describe "Thrór's Map" do
    it "makes a location in the staging area active in the travel phase" . dodTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.mountainsOfMirkwood
      run . AddToStaging =<< genEncounterCard Locations.theBrownLands
      locations <- getsGame (map toId . toList . (.entities.locations))
      let (first, second) = case locations of
            [a, b] -> (a, b)
            _ -> error "two locations"
      run $ PlaceLocation first ActiveLocation
      m <- putAttachmentOn pid Attachments.throrsMap =<< hero oriCode
      run $ BeginPhase TravelPhase
      chooseAbilityOf pid m
      chooseTarget pid second
      placements <- getsGame (\g -> [locationPlacement (toAttrs l) | lid <- [first, second], Just l <- [Map.lookup lid g.entities.locations]])
      placements `shouldBe` [StagingArea, ActiveLocation]

  describe "King Under the Mountain" do
    it "adds 1 of the top 2 cards to your hand and discards the other" . dodTest $ \pid -> do
      cards <- setDeck pid [Attachments.cram, Allies.bombur, Attachments.cram]
      king <- putAttachmentOn pid Attachments.kingUnderTheMountain =<< hero dainCode
      planning
      chooseAbilityOf pid king
      (first, second, third) <- case map (.id) cards of
        [a, b, c] -> pure (a, b, c)
        _ -> error "three cards"
      chooseTarget pid (CardIdTarget second)
      handIds pid >>= (`shouldSatisfy` elem second)
      discardIds pid `shouldReturn` [first]
      deckIds pid `shouldReturn` [third]

  describe "Dwarf Pipe" do
    it "places a card discarded from your deck on the bottom of it" . dodTest $ \pid -> do
      cards <- setDeck pid [Attachments.cram, Allies.bombur]
      pipe <- putAttachmentOn pid Attachments.dwarfPipe =<< hero dainCode
      run $ DiscardTopOfDeck pid 1
      chooseAbilityOf pid pipe
      deckIds pid `shouldReturn` reverse (map (.id) cards)

  describe "Magic Ring" do
    it "raises your threat by 1 to ready the attached hero" . dodTest $ \pid -> do
      dain <- hero dainCode
      ring <- putAttachmentOn pid Attachments.magicRing dain
      run $ Exhaust dain
      threat <- (.threat) <$> getPlayer pid
      planning
      chooseAbilityOf pid ring
      chooseLabel "$label.magicRing.ready" pid
      heroExhausted . toAttrs <$> getHeroWithCode dainCode `shouldReturn` False
      (.threat) <$> getPlayer pid `shouldReturn` (threat + 1)

  describe "A Very Good Tale" do
    it "puts discarded allies into play up to the cost of the allies exhausted" . dodTest $ \pid -> do
      zigil <- putAllyIntoPlay pid Allies.zigilMiner
      elder <- putAllyIntoPlay pid Allies.longbeardElder
      cards <- setDeck pid [Allies.dori, Allies.ereborRecordKeeper, Allies.bombur, Attachments.cram, Attachments.cram]
      _ <- playWith pid dainCode 0 Events.aVeryGoodTale
      chooseTarget pid zigil
      chooseTarget pid elder
      let byCode def = CardIdTarget $ (.id) $ fromJustNote "no card" $ find ((== toCardCode def) . toCardCode) cards
      chooseTarget pid (byCode Allies.dori)
      chooseTarget pid (byCode Allies.ereborRecordKeeper)
      allyCodes `shouldReturn` sort (map toCardCode [Allies.zigilMiner, Allies.longbeardElder, Allies.dori, Allies.ereborRecordKeeper])
