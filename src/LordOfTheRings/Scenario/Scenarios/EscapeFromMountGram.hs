{- | Escape from Mount Gram. The players start captured, each with one hero: see
"LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers" for the captured decks and
the Capture keyword, which the scenario resolves. At stage 2 each player has a staging
area and a Gornákh's Dungeons of his own, as at Foundations of Stone.
-}
module LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram (escapeFromMountGram) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Capture))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (AttackDamageDiscardsCaptured))
import LordOfTheRings.Source (Source (EnemyAttackSource))
import LordOfTheRings.Trait (Trait (Item))

newtype EscapeFromMountGram = EscapeFromMountGram ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromMountGram :: Scenario
escapeFromMountGram =
  Scenario
    $ scenario (EscapeFromMountGram . (nightmareDeckL ?~ nightmare)) "escape-from-mount-gram" "Escape from Mount Gram"
 where
  nightmare =
    nightmareDeck
      Nightmare.escapeFromMountGramNightmare
      EncounterSet.EscapeFromMountGramNightmare
      [ (Locations.prisonCell, 4)
      , (Enemies.dungeonGuard, 3)
      , (Enemies.jailorGornakh, 1)
      , (Treacheries.interrogation, 2)
      , (Enemies.goblinTormentor, 2)
      , (Enemies.cruelTorturer, 1)
      ]

-- | Setup: Set Southern Gate, Jailor Gornákh and the Angmar Orcs encounter set aside, out
-- of play (the nightmare Jailor Gornákh and Orc of Thaurdir, of the Angmar Orcs set, with
-- them). The rest of Unexpected Rescue's setup is its own.
--
-- Nightmare: "Forced: After choosing a starting hero and randomly setting another hero
-- aside facedown, instead of the standard captured deck setup, shuffle any remaining heroes
-- into your captured deck, then place the facedown set aside hero on the bottom of your
-- captured deck." Its other sentence, that the players cannot win while a hero is
-- captured, is Flight from Mount Gram's.
instance RunMessage EscapeFromMountGram where
  runMessage msg (EscapeFromMountGram attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.EscapeFromMountGram, EncounterSet.AngmarOrcs]
      let aside c =
            toCardCode c
              `elem` map toCardCode [Locations.southernGate, Enemies.jailorGornakh, Enemies.jailorGornakhNightmare, Enemies.orcOfThaurdir]
              || cdEncounterSet (toCardDef c) == Just EncounterSet.AngmarOrcs
          (setAsideCards, encounterDeck) = partition aside cards
      firstStage <- genEncounterCard Quests.unexpectedRescue
      laterStages <- traverse genEncounterCard (replicate 4 Quests.gornakhsDungeons <> [Quests.flightFromMountGram])
      putQuestIntoPlay firstStage
      pure
        $ EscapeFromMountGram
        $ attrs
        & encounterDeckL .~ encounterDeck
        & setAsideL .~ setAsideCards
        & questDeckL .~ laterStages
    -- the player shuffles his hand into his deck, and his allies and Item, Mount and
    -- Artifact attachments make up his captured deck
    ScenarioSpecific "mountGram.prepare" (parseMaybe parseJSON -> Just pid) -> do
      player <- getPlayer pid
      let capturable = [pc | PlayerCard pc <- player.hand <> player.deck, isCapturable pc]
          taken c = c.id `elem` map (.id) capturable
      for_ player.hand \c -> push $ if taken c then RemoveFromHand pid c.id else ShuffleFromHandIntoOwnersDeck pid c.id
      for_ (filter taken player.deck) \c -> removeFromDeck pid c.id
      deck <- shuffleM capturable
      pure $ withMeta meta {capturedDecks = Map.insert pid deck meta.capturedDecks}
    -- his other heroes are shuffled into his captured deck, a random one of them on top;
    -- he adds 2 resources to his starting hero and sets his threat to its threat cost
    ScenarioSpecific "mountGram.startingHero" (parseMaybe parseJSON -> Just (pid, hid)) -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      let others = filter ((/= hid) . toId) heroes
      for_ others (setAsideHero . toId)
      shuffled <- shuffleM [pc | PlayerCard pc <- map toCard others]
      let (top, rest) = splitAt 1 shuffled
      deck <- shuffleM (rest <> Map.findWithDefault [] pid meta.capturedDecks)
      let captured = if attrs.nightmare then deck <> top else top <> deck
      for_ (find ((== hid) . toId) heroes) \h -> do
        placeResources attrs h 2
        threat <- (.threat) <$> getPlayer pid
        raiseThreat pid (fromMaybe 0 (cdThreatCost $ toCardDef h) - threat)
      pure $ withMeta meta {capturedDecks = Map.insert pid captured meta.capturedDecks}
    -- Capture X, as the card enters the staging area, before its When Revealed effect
    CheckWindows ws -> do
      meta' <- foldM captureKeyword meta [t | Window After (EntersStagingArea t) <- ws]
      EscapeFromMountGram <$> runMessage msg (withMeta' meta')
    ScenarioSpecific "mountGram.capture" (parseMaybe parseJSON -> Just (t, n)) ->
      capturedTarget t >>= \case
        Just (host, card) -> withMeta <$> captureTop host card n meta
        Nothing -> pure $ EscapeFromMountGram attrs
    -- a character or attachment captured from play
    PlaceUnderneath t cards ->
      capturedTarget t >>= \case
        Just (_, host) -> do
          let held = Map.findWithDefault [] host.id meta.captured
              new = [pc | PlayerCard pc <- cards, pc.id `notElem` map (.id) held]
          unless (null new) $ raidCaptures host.id
          EscapeFromMountGram <$> runMessage msg (withMeta' meta {captured = Map.insert host.id (held <> new) meta.captured})
        Nothing -> EscapeFromMountGram <$> runMessage msg attrs
    ScenarioSpecific "mountGram.rescueAtRandom" (parseMaybe parseJSON -> Just (cid, n)) -> do
      chosen <- take n <$> shuffleM (Map.findWithDefault [] cid meta.captured)
      capturedTarget (CardIdTarget cid) >>= traverse_ \(_, host) -> rescue host chosen
      pure $ withMeta meta {captured = Map.adjust (filter (`notElem` chosen)) cid meta.captured}
    ScenarioSpecific "mountGram.rescue" (parseMaybe parseJSON -> Just (host, cards :: [PlayerCard])) -> do
      push $ CheckWindows [mkWhen $ CapturedCardsRescued host (map PlayerCard cards)]
      -- Raid Golfimbul's Vault: "Forced: When the captured cards underneath Raid
      -- Golfimbul's Vault are rescued, put each rescued Item card into its owner's hand and
      -- discard the rest."
      -- ponytail: a hero rescued from it still comes back into play, not discarded
      let vault = toCardCode host == toCardCode Quests.raidGolfimbulsVault
      for_ cards \pc -> case cdCardType (toCardDef pc) of
        HeroType -> putIntoPlayAtNoCost pc
        _ | vault && Item `notMember` cdCardTraits (toCardDef pc) -> push $ PutInDiscard pc
        _ -> putInHand pc
      push $ CheckWindows [mkAfter $ CapturedCardsRescued host (map PlayerCard cards)]
      -- Dungeon Guard: Response: After the captured card(s) underneath Dungeon Guard are
      -- rescued, the players may put 1 of them into play at no cost. It has left play, so
      -- the scenario offers it.
      when (toCardCode host == toCardCode Enemies.dungeonGuard) do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "dungeonGuard.putIntoPlay"
          showing (map PlayerCard cards)
          labeled "pass" $ pure ()
          for_ [pc | pc <- cards, cdCardType (toCardDef pc) /= HeroType] \pc ->
            targeting (CardIdTarget pc.id) do
              for_ pc.owner \owner -> push $ RemoveFromHand owner pc.id
              putIntoPlayAtNoCost pc
      pure $ EscapeFromMountGram attrs
    ScenarioSpecific "mountGram.rescueOne" (parseMaybe parseJSON -> Just (cid, one)) -> do
      let (chosen, rest) = partition ((== one) . (.id)) (Map.findWithDefault [] cid meta.captured)
      capturedTarget (CardIdTarget cid) >>= traverse_ \(_, host) -> rescue host chosen
      pure $ withMeta meta {captured = Map.insert cid rest meta.captured}
    ScenarioSpecific "mountGram.captureHeroes" (parseMaybe parseJSON -> Just t) ->
      capturedTarget t >>= \case
        Just (_, card) -> do
          let isHero = (== HeroType) . cdCardType . toCardDef
              heroes = concatMap (filter isHero) (Map.elems meta.capturedDecks)
          unless (null heroes) do
            placeUnderneath t (map PlayerCard heroes)
          pure
            $ withMeta
              meta
                { capturedDecks = Map.map (filter (not . isHero)) meta.capturedDecks
                , captured = Map.insertWith (flip (<>)) card.id heroes meta.captured
                }
        Nothing -> pure $ EscapeFromMountGram attrs
    -- Orc of Thaurdir's shadow: each point of the attack's damage discards the top card of
    -- the defending player's captured deck
    DoStep 1 (DealDamage (EnemyAttackSource eid) _ n) -> do
      mods <- getModifiers eid
      for_ [pid | AttackDamageDiscardsCaptured pid <- mods] \pid -> replicateM_ n (discardTopOfCapturedDeck pid)
      EscapeFromMountGram <$> runMessage msg attrs
    ScenarioSpecific "mountGram.release" (parseMaybe parseJSON -> Just (host, cid)) -> do
      let held = Map.findWithDefault [] host meta.captured
      traverse_ putIntoPlayAtNoCost (find ((== cid) . (.id)) held)
      pure $ withMeta meta {captured = Map.insert host (filter ((/= cid) . (.id)) held) meta.captured}
    ScenarioSpecific "mountGram.discardCaptured" (parseMaybe parseJSON -> Just host) -> do
      traverse_ (push . PutInDiscard) (Map.findWithDefault [] host meta.captured)
      pure $ withMeta meta {captured = Map.delete host meta.captured}
    ScenarioSpecific "mountGram.discardTop" (parseMaybe parseJSON -> Just pid) -> do
      let (top, rest) = splitAt 1 (Map.findWithDefault [] pid meta.capturedDecks)
      traverse_ (push . PutInDiscard) top
      pure $ withMeta meta {capturedDecks = Map.insert pid rest meta.capturedDecks}
    ScenarioSpecific "mountGram.shuffleIn" (parseMaybe parseJSON -> Just (pid, cards)) -> do
      deck <- shuffleM (cards <> Map.findWithDefault [] pid meta.capturedDecks)
      pure $ withMeta meta {capturedDecks = Map.insert pid deck meta.capturedDecks}
    -- the card leaving play, its captured cards are rescued
    Discard t -> leaving t
    Destroy _ t -> leaving t
    AddToVictoryDisplay t -> leaving t
    ShuffleIntoEncounterDeck t -> leaving t
    ReturnToTopOfEncounterDeck t -> leaving t
    ReturnToBottomOfEncounterDeck t -> leaving t
    RemoveFromGame t -> leaving t
    CompleteQuest qid -> leaving (QuestTarget qid)
    _ -> EscapeFromMountGram <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta = EscapeFromMountGram . withMeta'
    withMeta' m = attrs & metaL .~ toJSON m
    captureKeyword m t =
      capturedTarget t >>= \case
        Just (host, card) -> do
          n <- keywordValue (\case Capture k -> Just k; _ -> Nothing) host (toCardDef card)
          captureTop host card n m
        Nothing -> pure m
    -- each player at the card's stage captures the top cards of his captured deck: they
    -- are stacked at once, the message they are placed with telling the log
    captureTop host card n m = do
      players <- getRemainingPlayers
      let takeFrom (m', acc) pid =
            let (top, rest) = splitAt n (Map.findWithDefault [] pid m'.capturedDecks)
             in (m' {capturedDecks = Map.insert pid rest m'.capturedDecks}, acc <> top)
          (m'', taken) = foldl' takeFrom (m, []) players
      unless (null taken) do
        placeUnderneath host (map PlayerCard taken)
        raidCaptures card.id
      pure m'' {captured = Map.insertWith (flip (<>)) card.id taken m''.captured}
    -- Raid Golfimbul's Vault: "Forced: After a card is captured by another card effect,
    -- capture a random card from each player's hand underneath Raid Golfimbul's Vault."
    raidCaptures hostId = do
      quests <- getsGame (toList . entitiesQuests . gameEntities)
      for_ [q | q <- quests, toCardCode q == toCardCode Quests.raidGolfimbulsVault, (toAttrs q).cardId /= hostId] \vault ->
        eachPlayer \p -> do
          hand <- (.hand) <$> getPlayer p
          picked <- take 1 <$> shuffleM [pc | PlayerCard pc <- hand]
          captureFromHand p (toTarget vault) picked
    rescue host cards = unless (null cards) $ push $ ScenarioSpecific "mountGram.rescue" (toJSON (host, cards))
    leaving t =
      capturedTarget t >>= \case
        Just (_, host) | notNull held -> do
          rescue host held
          EscapeFromMountGram <$> runMessage msg (withMeta' meta {captured = Map.delete host.id meta.captured})
         where
          held = Map.findWithDefault [] host.id meta.captured
        _ -> EscapeFromMountGram <$> runMessage msg attrs
