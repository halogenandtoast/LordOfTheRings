module LordOfTheRings.VengeanceOfMordor.ContractsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (CardDef (..), CardType (AttachmentType), cdName, toCard, toCardDef)
import LordOfTheRings.Contract (Contract, ContractAttrs (..))
import LordOfTheRings.Contract.CardDefs qualified as Contracts
import LordOfTheRings.Decklist (deckProblem, loadPlayer, withContract)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Frontend (FrontendMessage (..))
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Log.Entry
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Matcher (CardMatcher (..), playerCardsGuardedBy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Phase (Phase (PlanningPhase, RefreshPhase))
import LordOfTheRings.Sphere (Sphere (Baggins, Fellowship))
import LordOfTheRings.Trait (Trait (Artifact, Boon, Item))
import LordOfTheRings.Window (WindowType (Explored, PlayedCardFromHand), mkAfter, mkWhen)
import TestImport

beravor, eowyn, theodred, gimli :: CardCode
beravor = "01012"
eowyn = "01007"
theodred = "01002"
gimli = "01004"

contractCode :: CardDef -> CardCode
contractCode = toCardCode

getContract :: GameT Contract
getContract = getsGame (fromJustNote "no contract" . headMay . toList . (.entities.contracts))

contractAttrs :: GameT ContractAttrs
contractAttrs = toAttrs <$> getContract

flipped :: GameT Bool
flipped = contractFlipped <$> contractAttrs

heroTarget :: CardCode -> GameT Target
heroTarget code = toTarget <$> getHeroWithCode code

-- | Answer every question the player is asked with its first choice, until none is left.
firstChoices :: PlayerId -> Int -> GameT ()
firstChoices pid n = when (n > 0) $ getQuestion pid >>= \case
  Just _ -> answerQuestion pid 0 >> firstChoices pid (n - 1)
  Nothing -> pure ()

-- | A deck of exactly 50 cards, 10 from each of the four spheres and 10 neutral, 2 copies
-- of each title, with a hero of each sphere: as Bond of Friendship asks.
bondDeck :: DeckList
bondDeck = bondWith [theodred, beravor, eowyn, gimli] bondCards

bondWith :: [CardCode] -> [(CardCode, Int)] -> DeckList
bondWith hs = withContract (contractCode Contracts.bondOfFriendship) . deckList hs

bondCards :: [(CardCode, Int)]
bondCards = [(code, 2) | code <- leadership <> tactics <> spirit <> lore <> neutral]
 where
  leadership = ["01013", "01014", "01015", "01016", "01017"]
  tactics = ["01028", "01029", "01030", "01031", "01032"]
  spirit = ["01043", "01044", "01045", "01046", "01047"]
  lore = ["01058", "01059", "01060", "01061", "01062"]
  neutral = ["01073", "05018", "08065", "12066", "06041"]

loggedTestWith :: DeckList -> (IORef [LogEntry] -> PlayerId -> GameT ()) -> IO ()
loggedTestWith deck body = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer "tester" deck
  logged <- newIORef []
  env <- newGameEnv (newGame player heroes) (\case Logged e -> modifyIORef' logged (e :); _ -> pure ()) 1
  runGameT env (body logged player.id)

logKeys :: IORef [LogEntry] -> GameT [Text]
logKeys logged = mapMaybe key . reverse <$> readIORef logged
 where
  key e = case e.logEntryBody of
    LogI18n k _ : _ -> Just k
    _ -> Nothing

-- | A test that reads no game.
check :: IO () -> IO ()
check = id

-- | Whether The Grey Wanderer now lets the player's next non-unique card off its match.
lets :: PlayerId -> GameT Bool
lets pid = elem (NextIgnoresResourceMatch (NotCard UniqueCard)) <$> getModifiers (PlayerTarget pid)

spec :: Spec
spec = describe "Contracts" do
  describe "deck lists" do
    it "takes a contract among a RingsDB deck's cards as its contract, not one of its cards" $ check do
      let dl = deckList [beravor] [("01013", 2), (contractCode Contracts.fellowship, 1)]
      dl.contract `shouldBe` Just (contractCode Contracts.fellowship)
      dl.cards `shouldBe` [("01013", 2)]

    it "refuses a second contract" $ check do
      let dl = deckList [beravor] [(contractCode Contracts.fellowship, 1), (contractCode Contracts.theGreyWanderer, 1)]
      deckProblem dl `shouldSatisfy` isJust

    it "puts the contract into play with the heroes, side A faceup" . gameTestWith (withContract (contractCode Contracts.fellowship) testDeck) $ \pid -> do
      c <- contractAttrs
      c.contractCardCode `shouldBe` contractCode Contracts.fellowship
      c.contractOwner `shouldBe` pid
      c.contractPlacement `shouldBe` InPlayArea pid
      c.contractFlipped `shouldBe` False
      -- not in the deck
      p <- getPlayer pid
      any ((== contractCode Contracts.fellowship) . toCardCode) p.deck `shouldBe` False

    it "flips to side B and back" . gameTestWith (withContract (contractCode Contracts.fellowship) testDeck) $ \_ -> do
      c <- getContract
      run $ Flip (toTarget c)
      flipped `shouldReturn` True
      run $ Flip (toTarget c)
      flipped `shouldReturn` False

    it "narrates a contract's flips" . loggedTestWith (withContract (contractCode Contracts.fellowship) testDeck) $ \logged _ -> do
      c <- getContract
      run $ Flip (toTarget c)
      run $ Flip (toTarget c)
      logged' <- logKeys logged
      filter (`elem` ["log.contractFlipsToA", "log.contractFlipsToB"]) logged' `shouldBe` ["log.contractFlipsToB", "log.contractFlipsToA"]

  describe "Fellowship" do
    let deck = withContract (contractCode Contracts.fellowship) testDeck
    it "side A bars playing or putting non-unique allies into play, not unique ones" . gameTestWith deck $ \pid -> do
      putGame =<< preloadModifiers =<< getGame
      card <- putCardIntoHand pid Allies.guardOfTheCitadel
      publicHandBlockers card `shouldReturn` Just [RefContract]
      run $ PutAllyIntoPlay pid card.id
      getsGame (Map.size . (.entities.allies)) `shouldReturn` 0
      _ <- putAllyIntoPlay pid Allies.faramir
      getsGame (Map.size . (.entities.allies)) `shouldReturn` 1

    it "flips once you control exactly 9 unique characters, and side B bars every ally and raises your characters" . gameTestWith deck $ \pid -> do
      for_ [Allies.faramir, Allies.brokIronfist, Allies.beorn, Allies.henamarthRiversong, Allies.gleowine] (void . putAllyIntoPlay pid)
      flipped `shouldReturn` False
      haldir <- putAllyIntoPlay pid Allies.haldirOfLorien
      flipped `shouldReturn` True
      -- Beravor's 2 willpower, +1
      (`shouldBe` 3) =<< getWillpower =<< heroTarget beravor
      getWillpower haldir `shouldReturn` 3
      card <- putCardIntoHand pid Allies.wanderingTook
      publicHandBlockers card `shouldReturn` Just [RefContract]
      -- Forced: After a character you control leaves play, flip this card over.
      run $ Discard GameSource haldir
      flipped `shouldReturn` False
      (`shouldBe` 2) =<< getWillpower =<< heroTarget beravor

  describe "The Burglar's Turn" do
    let deck = withContract (contractCode Contracts.theBurglarsTurn) (deckList [beravor, eowyn, theodred] [("01013", 3), ("01045", 3)])
    it "refuses a deck with an attachment" $ check do
      deckProblem (withContract (contractCode Contracts.theBurglarsTurn) testDeck) `shouldSatisfy` isJust
      deckProblem deck `shouldBe` Nothing

    it "sets up a loot deck of 14 different Item or Artifact attachments under it, flipped" . gameTestWith deck $ \pid -> do
      run Setup
      firstChoices pid 14
      flipped `shouldReturn` True
      loot <- (.contractUnderneath) <$> contractAttrs
      length loot `shouldBe` 14
      let title c = let Name t _ = cdName (toCardDef c) in t
      length (ordNub $ map title loot) `shouldBe` 14
      for_ loot \c -> do
        (toCardDef c).cdCardType `shouldBe` AttachmentType
        any (`member` (toCardDef c).cdCardTraits) [Item, Artifact] `shouldBe` True

    -- Caleb, unofficially: boons and saga attachments are not meant for the loot deck
    it "leaves boons, the sagas' Fellowship and Baggins attachments and treasures out of the collection" . gameTestWith deck $ \pid -> do
      run Setup
      offered <- maybe [] (\q -> [toCardDef c | TargetLabel _ msgs <- questionChoices q, PlaceUnderneath _ cs <- msgs, c <- cs]) <$> getQuestion pid
      offered `shouldSatisfy` notNull
      offered `shouldSatisfy` all (\d -> d.cdSphere `notElem` [Just Fellowship, Just Baggins] && isNothing d.cdSetIcon && Boon `notMember` d.cdCardTraits)

    it "attaches loot to each location traveled to, adds a quest point for it, and lets it be taken as the location is explored" . gameTestWith deck $ \pid -> do
      run Setup
      firstChoices pid 14
      chooseLabel "$label.keepHand" pid
      chooseOptionMatching "flip the quest" pid (const True)
      top <- fromJustNote "no loot" . headMay . (.contractUnderneath) <$> contractAttrs
      -- the quest's setup adds Old Forest Road to the staging area
      lid <- getsGame (fromJustNote "no location" . headMay . Map.keys . (.entities.locations))
      printed <- getLocationQuestPoints lid
      run $ TravelTo lid
      -- Old Forest Road's own response to the travel
      chooseLabel "$label.pass" pid
      l <- getLocation lid
      held <- playerCardsGuardedBy (toCard l).id
      map (.id) held `shouldBe` [top.id]
      length . (.contractUnderneath) <$> contractAttrs `shouldReturn` 13
      getLocationQuestPoints lid `shouldReturn` printed + 1
      -- Forced: When the active location is explored, ... or add it to your hand.
      run $ CheckWindows [mkWhen $ Explored lid]
      chooseLabel "$label.theBurglarsTurn.toHand" pid
      hand <- (.hand) <$> getPlayer pid
      any ((== top.id) . (.id)) hand `shouldBe` True
      playerCardsGuardedBy (toCard l).id `shouldReturn` []

  describe "Forth, The Three Hunters!" do
    let deck = withContract (contractCode Contracts.forthTheThreeHunters) (deckList [beravor, eowyn, theodred] [("01027", 1), ("01039", 3)])
    it "refuses a deck with an ally" $ check do
      deckProblem (withContract (contractCode Contracts.forthTheThreeHunters) testDeck) `shouldSatisfy` isJust
      deckProblem deck `shouldBe` Nothing

    it "lets each hero have a third restricted attachment, not a fourth" . gameTestWith deck $ \pid -> do
      h <- heroTarget eowyn
      for_ [Attachments.bladeOfGondolin, Attachments.citadelPlate, Attachments.hornOfGondor] \def -> putAttachmentOn pid def h
      run $ CheckRestricted h
      getQuestion pid `shouldReturn` Nothing
      _ <- putAttachmentOn pid Attachments.dwarvenAxe h
      run $ CheckRestricted h
      (`shouldSatisfy` isJust) =<< getQuestion pid

    it "reduces the first restricted attachment played on each hero each round by 1" . gameTestWith deck $ \pid -> do
      putGame =<< preloadModifiers =<< getGame
      h <- heroTarget theodred
      elem (ReduceCostToAttach (CardWithType AttachmentType <> CardWithKeyword Restricted) 1) <$> getModifiers h `shouldReturn` True
      _ <- playWith pid theodred 1 Attachments.celebriansStone
      chooseTarget pid h
      (`shouldBe` 0) =<< getHeroResources . toId =<< getHeroWithCode theodred
      elem (ReduceCostToAttach (CardWithType AttachmentType <> CardWithKeyword Restricted) 1) <$> getModifiers h `shouldReturn` False

    it "flips by its Refresh Action once each hero has 2 restricted attachments; side B adds willpower and heals" . gameTestWith deck $ \pid -> do
      heroes <- traverse heroTarget [beravor, eowyn, theodred]
      for_ heroes \h -> for_ [Attachments.bladeOfGondolin, Attachments.hornOfGondor] \def -> putAttachmentOn pid def h
      c <- getContract
      let offered = any ((== toSource c) . (.abilitySource)) <$> getActionAbilities pid
      offered `shouldReturn` False
      g <- getGame
      putGame g {gamePhase = RefreshPhase}
      offered `shouldReturn` True
      run $ UseCardAbility pid (toSource c) 1 []
      flipped `shouldReturn` True
      -- Beravor's 2 willpower, +1 for each of her 2 restricted attachments
      (`shouldBe` 4) =<< getWillpower =<< heroTarget beravor
      for_ heroes \h -> run $ PlaceTokens GameSource h Damage 2
      run $ UseCardAbility pid (toSource c) 2 []
      for_ heroes \h -> getCharacterDamage h `shouldReturn` 1

  describe "The Grey Wanderer" do
    let deck = withContract (contractCode Contracts.theGreyWanderer) (deckList [gimli] [("01039", 1), ("01045", 3)])
    it "refuses more than 1 starting hero" $ check do
      deckProblem (withContract (contractCode Contracts.theGreyWanderer) testDeck) `shouldSatisfy` isJust
      deckProblem deck `shouldBe` Nothing

    it "puts an attachment with a printed cost of 1 from the deck into play at setup, and flips" . gameTestWith deck $ \pid -> do
      run Setup
      firstChoices pid 2
      flipped `shouldReturn` True
      h <- heroTarget gimli
      getsGame (any (\a -> toCardCode a == toCardCode Attachments.bladeOfGondolin && a.placement == AttachedTo h) . toList . (.entities.attachments))
        `shouldReturn` True

    it "side B: the first non-unique card played each planning phase needs no resource match" . gameTestWith deck $ \pid -> do
      c <- getContract
      run $ Flip (toTarget c)
      -- Northern Tracker is Spirit, Gimli Tactics
      card <- playWith pid gimli 4 Allies.northernTracker
      getsGame (any ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies)) `shouldReturn` True
      -- spent: the next one needs its match again
      lets pid `shouldReturn` False

    it "side B: only in the planning phase, spent by a non-unique card played at no cost too" . gameTestWith deck $ \pid -> do
      c <- getContract
      run $ Flip (toTarget c)
      lets pid `shouldReturn` False
      -- in the planning phase, outside its action window
      g <- getGame
      putGame g {gamePhase = PlanningPhase}
      putGame =<< preloadModifiers =<< getGame
      lets pid `shouldReturn` True
      -- a unique card leaves it
      faramir <- genPlayerCard Allies.faramir (Just pid)
      run $ PlayAtNoCost pid faramir
      lets pid `shouldReturn` True
      tracker <- genPlayerCard Allies.northernTracker (Just pid)
      run $ PlayAtNoCost pid tracker
      lets pid `shouldReturn` False

    it "side B: exhausts and raises threat to resolve two of its options on the starting hero" . gameTestWith deck $ \pid -> do
      c <- getContract
      run $ Flip (toTarget c)
      h <- heroTarget gimli
      run $ Exhaust h
      run $ PlaceTokens GameSource h Damage 3
      threat <- (.threat) <$> getPlayer pid
      ab <- fromJustNote "no action" . find ((== toSource c) . (.abilitySource)) <$> getActionAbilities pid
      run $ UseAbility pid ab []
      chooseLabel "$label.theGreyWanderer.ready" pid
      chooseLabel "$label.theGreyWanderer.heal" pid
      (.threat) <$> getPlayer pid `shouldReturn` threat + 1
      (.exhausted) <$> contractAttrs `shouldReturn` True
      getCharacterDamage h `shouldReturn` 0
      heroExhausted . toAttrs <$> getHeroWithCode gimli `shouldReturn` False

  describe "Council of the Wise" do
    let deck = withContract (contractCode Contracts.councilOfTheWise) (deckList [beravor, eowyn, theodred] [("01013", 1), ("01014", 1)])
    it "refuses a second copy of a title" $ check do
      deckProblem (withContract (contractCode Contracts.councilOfTheWise) testDeck) `shouldSatisfy` isJust
      deckProblem deck `shouldBe` Nothing

    it "answers an event played from hand with one option, each once per round" . gameTestWith deck $ \pid -> do
      c <- getContract
      let played = run $ CheckWindows [mkAfter $ PlayedCardFromHand pid (toCardCode Events.commonCause)]
      run $ RaiseThreat pid 10
      threat <- (.threat) <$> getPlayer pid
      played
      chooseAbilityOf pid c
      chooseLabel "$label.councilOfTheWise.threat" pid
      (.threat) <$> getPlayer pid `shouldReturn` threat - 1
      played
      chooseAbilityOf pid c
      -- reducing threat is spent for the round
      q <- fromJustNote "no question" <$> getQuestion pid
      [l | Label l _ <- questionChoices q] `shouldBe` ["$label.councilOfTheWise.draw", "$label.councilOfTheWise.resource"]
      chooseLabel "$label.councilOfTheWise.draw" pid
      played
      chooseAbilityOf pid c
      chooseLabel "$label.councilOfTheWise.resource" pid
      chooseTarget pid =<< heroTarget beravor
      (`shouldBe` 1) =<< getHeroResources . toId =<< getHeroWithCode beravor
      -- every option is spent: no response is offered
      played
      offersAbilityOf pid c `shouldReturn` False
      run EndRound
      (.contractMeta) <$> contractAttrs `shouldReturn` Null

  describe "Messenger of the King" do
    let deck = withContract (contractCode Contracts.messengerOfTheKing) (deckList [beravor, eowyn] [("01014", 1), ("01013", 3)])
    it "refuses more than 2 heroes" $ check do
      deckProblem (withContract (contractCode Contracts.messengerOfTheKing) testDeck) `shouldSatisfy` isJust
      deckProblem deck `shouldBe` Nothing

    it "puts a unique ally from the deck into play as a hero at setup, its stats added to the starting threat" . gameTestWith deck $ \pid -> do
      run Setup
      threat <- (.threat) <$> getPlayer pid
      chooseOptionMatching "choose Faramir" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      faramir <- getHeroWithCode (toCardCode Allies.faramir)
      getsGame (Map.size . (.entities.allies)) `shouldReturn` 0
      -- Faramir's printed 2 willpower, 1 attack, 2 defense and 3 hit points
      p <- getPlayer pid
      p.threat `shouldBe` threat + 8
      p.startingThreat `shouldBe` threat + 8
      c <- contractAttrs
      c.contractFlipped `shouldBe` True
      c.contractPlacement `shouldBe` AttachedTo (toTarget faramir)
      elem ImmuneToCardEffects <$> getModifiers c.contractId `shouldReturn` True

    it "side B: its character readies at most once a phase, and it leaves the game with it" . gameTestWith deck $ \pid -> do
      run Setup
      chooseOptionMatching "choose Faramir" pid \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      chooseLabel "$label.keepHand" pid
      chooseOptionMatching "flip the quest" pid (const True)
      faramir <- toTarget <$> getHeroWithCode (toCardCode Allies.faramir)
      let exhausted = heroExhausted . toAttrs <$> getHeroWithCode (toCardCode Allies.faramir)
      run $ Exhaust faramir
      run $ Ready faramir
      exhausted `shouldReturn` False
      run $ Exhaust faramir
      run $ Ready faramir
      exhausted `shouldReturn` True
      run $ Discard GameSource faramir
      getsGame (Map.size . (.entities.contracts)) `shouldReturn` 0

  describe "Bond of Friendship" do
    it "allows 4 heroes of different spheres, with exactly 10 cards of each sphere in a deck of exactly 50" $ check do
      deckProblem bondDeck `shouldBe` Nothing
      -- two Leadership heroes
      deckProblem (bondWith [theodred, beravor, eowyn, "01001"] bondCards) `shouldSatisfy` isJust
      -- 49 cards
      deckProblem (bondWith [theodred, beravor, eowyn, gimli] (drop 1 bondCards)) `shouldSatisfy` isJust
      -- 3 copies of a title
      deckProblem (bondWith [theodred, beravor, eowyn, gimli] (("01013", 3) : ("01014", 1) : drop 2 bondCards)) `shouldSatisfy` isJust
      deckProblem (deckList [theodred, beravor, eowyn, gimli] []) `shouldSatisfy` isJust

    it "plays with 4 heroes" . gameTestWith bondDeck $ \_ -> do
      getsGame (Map.size . (.entities.heroes)) `shouldReturn` 4
