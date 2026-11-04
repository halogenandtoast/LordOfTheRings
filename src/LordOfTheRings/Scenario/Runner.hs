{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario.Runner (module LordOfTheRings.Scenario.Runner, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card hiding (hero)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.EncounterSet (EncounterSet (ThePowerOfMordor))
import LordOfTheRings.I18n
import LordOfTheRings.Keyword (Keyword (Encounter, Guarded, GuardedBy, LastPlayerGainsControl, Permanent))
import LordOfTheRings.Trait (Trait (RingBearer))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (chooseOneM, placeResources, questionLabeled, removeFromDeck, showing, shuffleIntoDeck, targets)
import LordOfTheRings.Campaign
import LordOfTheRings.Game.Base (allPlayers, gameEntities, gamePhase, gamePlayerOrder, gamePlayers, gameRound)
import LordOfTheRings.Entities (entitiesHeroes, entitiesObjectives)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (EncounterDeckDoesNotReset, EncounterDeckResetsWheneverEmpty, HideValue, RecordedIfWon, ReturnBeforeEncounterDeckResets, ShadowCardsDealt))
import LordOfTheRings.Outcome
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (GuardRelease (..), Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Sailing (discardEncounterCard, runSailing)
import LordOfTheRings.Saga (removedBurdenSets)
import LordOfTheRings.Scenario.Types as X
import LordOfTheRings.Source (Source (ObjectiveSource), sourceTarget)
import LordOfTheRings.Target
import LordOfTheRings.Window (WindowType (AdvancedToStage, DiscardedFromEncounterDeck, LookedAtEncounterDeck), mkAfter)

-- | The copies of each card in the set, fewer of some in easy mode.
gatherEncounterSet :: MonadRandom m => ScenarioAttrs -> EncounterSet -> m [EncounterCard]
gatherEncounterSet attrs encounterSet =
  concatMapM
    (\def -> replicateM (copies def) (genEncounterCard def))
    -- quest stages share the set but make up the quest deck, never the encounter deck, and
    -- a nightmare setup card goes beside the quest deck
    $ filter
      (\def -> cdEncounterSet def == Just encounterSet && cdCardType def `notElem` [QuestType, NightmareType])
      (toList allEncounterCards)
 where
  copies def =
    fromMaybe 1 $ (guard attrs.easyMode *> cdEasyModeQuantity def) <|> cdEncounterSetQuantity def

-- | Whether a boon or burden of the Campaign Pool goes into the scenario: not one of a burden
-- set its campaign card removes (see 'removedBurdenSets').
keptBurden :: ScenarioAttrs -> CardDef -> Bool
keptBurden attrs def = maybe True (`notElem` removedBurdenSets attrs.scenarioId) (cdSetIcon def)

-- | A card of the Campaign Pool that comes into play as the scenario is set up rather than
-- being shuffled into a deck: each objective, a saga objective-ally boon (with a set icon),
-- as Army of the Dead, and a burden whose Setup adds it to the staging area, as Wraith on
-- Wings.
entersPlayAtSetup :: CardDef -> Bool
entersPlayAtSetup def = cdCardType def == ObjectiveType || (cdCardType def == ObjectiveAllyType && isJust (cdSetIcon def)) || cdSetupToStaging def

-- | The scenario's encounter deck from its encounter sets. In nightmare mode the
-- nightmare deck's removals come out and its nightmare set goes in.
gatherEncounterSets :: MonadRandom m => ScenarioAttrs -> [EncounterSet] -> m [EncounterCard]
gatherEncounterSets attrs sets = case attrs.nightmareDeck of
  Just nd | attrs.nightmare -> do
    deck <- concatMapM (gatherEncounterSet attrs) (sets <> [nightmareSet nd])
    pure $ foldl' removeCopies deck (nightmareRemovals nd)
  _ -> concatMapM (gatherEncounterSet attrs) sets
 where
  removeCopies deck (code, n) =
    let (copies, rest) = partition ((== code) . toCardCode) deck in drop n copies <> rest

-- | The nightmare set's replacement for a standard card, as a quest stage, in nightmare mode.
nightmareOr :: ScenarioAttrs -> CardDef -> CardDef -> CardDef
nightmareOr attrs standard replacement = if attrs.nightmare then replacement else standard

instance RunMessage ScenarioAttrs where
  runMessage msg attrs = resetEmptyEncounterDeck =<< case msg of
    -- the scenario gathers its encounter deck at step 1; shuffle it before any setup reveals
    -- in campaign mode the encounter cards of the Campaign Pool are shuffled in with it
    DoStep 2 Setup -> do
      -- (an objective, as Arnor Ravaged, comes into play as its own Setup text says)
      earned <- traverse genEncounterCard [def | p <- foldMap (.pool) attrs.campaign, isNothing p.attachedTo, isNothing p.seat, let def = toCardDef p.code, not (isPlayerCardDef def), not (entersPlayAtSetup def), keptBurden attrs def]
      encounterDeck <- shuffleM (earned <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ encounterDeck
    -- once the starting hands are drawn, an encounter card of the pool with a seat goes
    -- into that seat's deck, as Temple of the Deceived's resolution has a Recurring
    -- Nightmare shuffled in
    DoStep 6 Setup -> do
      order <- getsGame gamePlayerOrder
      for_ [(pid, def) | p <- foldMap (.pool) attrs.campaign, isNothing p.attachedTo, Just pid <- [atMay order =<< p.seat], let def = toCardDef p.code, not (isPlayerCardDef def), not (entersPlayAtSetup def), keptBurden attrs def] \(pid, def) ->
        shuffleIntoDeck pid =<< genEncounterCard def
      pure attrs
    DoStep 4 Setup -> do
      heroes <- getsGame (toList . entitiesHeroes . gameEntities)
      when attrs.easyMode $ for_ heroes \hero -> placeResources attrs hero.id 1
      -- the Permanent boons and burdens go on the heroes the log records them on
      for_ (foldMap (.pool) attrs.campaign) \p ->
        for_ [h | Just code <- [p.attachedTo], h <- heroes, toCardCode h == code] \hero -> do
          let def = toCardDef p.code
          if isPlayerCardDef def
            then do
              card <- genPlayerCard def (Just hero.owner)
              push $ PlayAttachmentOn hero.owner card (toTarget hero.id)
            else do
              card <- genEncounterCard def
              push $ PutObjectiveIntoPlayAttached card (toTarget hero.id)
      -- a saga burden of the pool comes into play as its Setup reads: to the staging area,
      -- as Gandalf's Delay and Wraith on Wings, onto the threat dial of the player the log
      -- records it for, as Overcome by Fear, or, a Permanent objective, attached to the
      -- Ring-bearer, as A Heavy Burden ("Setup: Attach to the Ring-bearer"): not used without
      -- one in play; a saga objective-ally boon under the control its text gives, as Army of
      -- the Dead's "Setup: The first player takes control"
      order <- getsGame gamePlayerOrder
      ringBearers <- select (HeroWithTrait RingBearer)
      for_ [p | p <- foldMap (.pool) attrs.campaign, isNothing p.attachedTo, let def = toCardDef p.code, isJust (cdSetIcon def), entersPlayAtSetup def, keptBurden attrs def] \p -> do
        card <- genEncounterCard (toCardDef p.code)
        let def = toCardDef p.code
        if cdCardType def == ObjectiveAllyType
          then do
            controller <- if LastPlayerGainsControl `member` cdKeywords def then getLastPlayer else getLead
            push $ PutObjectiveAllyIntoPlayUnder controller card
          else case atMay order =<< p.seat of
            _ | cdCardType def == ObjectiveType, Permanent `member` cdKeywords def -> for_ (take 1 ringBearers) (push . PutObjectiveIntoPlayAttached card . toTarget)
            Just pid -> push $ PutObjectiveIntoPlayAttached card (PlayerTarget pid)
            Nothing -> push $ AddToStaging card
      -- a card of a player's deck reading "Setup: Attach to a hero in play" is attached to
      -- the hero its owner chooses
      for_ order \pid -> do
        player <- getPlayer pid
        for_ [pc | PlayerCard pc <- player.deck, cdSetupAttach (toCardDef pc)] \pc -> do
          removeFromDeck pid pc.id
          chooseOneM pid do
            questionLabeled "setupAttach"
            showing [PlayerCard pc]
            targets heroes \hero -> push $ PlayAttachmentOn pid pc (toTarget hero.id)
      pure attrs
    -- what every campaign records once a scenario is won: the score, and the heroes in
    -- their owners' discard piles fallen (Revised Core rulebook, "Scoring" and "The
    -- Fellowship of Heroes")
    UpdateCampaignLog u -> pure $ attrs & campaignL %~ fmap (updateLog u)
    ResolveCampaign | Just l0 <- attrs.campaign -> do
      -- what the cards in play record should the scenario be won, as Determination's
      -- Forced effect marks its character
      gameMods <- getModifiers GameTarget
      let mark lg (key, code) = updateLog (Record key (ordNub $ recorded key lg <> [code])) lg
          l = foldl' mark l0 [(key, code) | RecordedIfWon key code <- gameMods]
      players <- getsGame (toList . allPlayers)
      heroes <- getsGame (toList . entitiesHeroes . gameEntities)
      rounds <- getsGame gameRound
      let inPlay = map toCardCode heroes
          fallen = [code | p <- players, code <- p.startingHeroes, code `notElem` inPlay]
          score =
            sum (map (.threat) players)
              + sum [fromMaybe 0 (cdThreatCost $ toCardDef code) | code <- fallen]
              + sum (map (.damage) heroes)
              + 10 * rounds
              - attrs.victoryPoints
          l' = recordFallen fallen l {campaignLogCompleted = l.completed <> [CompletedScenario attrs.scenarioId score]}
      pure $ attrs & campaignL ?~ l' & campaignCardFlippedL .~ True
    ShuffleDeck EncounterDeck -> do
      encounterDeck <- shuffleM attrs.encounterDeck
      pure $ attrs & encounterDeckL .~ encounterDeck
    Search pid EncounterDeck cardMatcher ToStaging Shuffle -> do
      let matches = filter (cardMatches cardMatcher) attrs.encounterDeck
      case matches of
        [] -> pure attrs
        (x : xs) ->
          if all ((== toCardCode x) . toCardCode) xs
            then do
              encounterDeck <- shuffleM $ filter ((/= x.id) . (.id)) attrs.encounterDeck
              push $ AddToStaging x
              pure $ attrs & encounterDeckL .~ encounterDeck
            else do
              push
                $ Ask pid
                $ QuestionLabel (ikey "label.searchChoose") Nothing []
                $ SearchCards (map EncounterCard matches)
                $ ChooseOne
                  [targetLabel y [RemoveFromEncounterDeck y, AddToStaging y, ShuffleDeck EncounterDeck] | y <- matches]
              pure attrs
    RevealEncounterCard -> case attrs.encounterDeck of
      (card : rest) -> do
        push $ RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      [] -> pure attrs
    -- an enemy dealt more than one whenever it is dealt one, as Fram's Shade
    DealShadowCard eid -> do
      n <- fromMaybe 1 . maximumMay . (\ms -> [k | ShadowCardsDealt k <- ms]) <$> getModifiers (EnemyTarget eid)
      let (dealt, rest) = splitAt n attrs.encounterDeck
      pushAll $ map (AttachShadowCard eid) dealt
      pure $ attrs & encounterDeckL .~ rest
    LookAtTopOfEncounterDeck pid source canMove -> do
      for_ (headMay attrs.encounterDeck) \top ->
        pushAll
          $ (: [CheckWindows [mkAfter $ LookedAtEncounterDeck pid]])
          $ Ask pid
          $ QuestionLabel (ikey "label.topOfEncounterDeck") (sourceTarget source) []
          $ SearchCards [EncounterCard top]
          $ ChooseOne
          $ if canMove
            then
              [ Label (ikey "label.leaveOnTop") []
              , Label (ikey "label.moveToBottom") [MoveTopOfEncounterDeckToBottom]
              ]
            else [Label (ikey "label.continue") []]
      pure attrs
    RevealOneOfTopEncounterCards pid source n toBottom -> do
      let (top, rest) = splitAt n attrs.encounterDeck
          others c = filter (/= c) top
      unless (null top)
        $ pushAll
        $ (: [CheckWindows [mkAfter $ LookedAtEncounterDeck pid]])
        $ Ask pid
        $ QuestionLabel (ikey $ if toBottom then "label.revealOneRestToBottom" else "label.revealOne") (sourceTarget source) []
        $ SearchCards (map EncounterCard top)
        $ ChooseOne
          [ targetLabel c [RevealCard c, if toBottom then PutOnBottomOfEncounterDeck (others c) else AddToEncounterDiscard (others c)]
          | c <- top
          ]
      pure $ attrs & encounterDeckL .~ rest
    PutOnBottomOfEncounterDeck cards -> pure $ attrs & encounterDeckL %~ (<> cards)
    DiscardTopOfEncounterDeck source target n -> do
      (attrs', discarded) <- discardFrom n attrs []
      push $ DiscardedTopOfEncounterDeck source target discarded
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromEncounterDeck c.id | c <- discarded]
      pure attrs'
    -- the Hide X value as modified now, as Woody End lowers it; once the cards are
    -- discarded the players may act before the test resolves (The Black Riders rules
    -- insert, "Player Actions During Hide Tests")
    DiscardForHideTest t -> do
      mods <- (<>) <$> getModifiers GameTarget <*> getModifiers t.hidePlayer
      (attrs', discarded) <- discardFrom (max 0 $ t.hideValue + sum [n | HideValue n <- mods]) attrs []
      order <- getsGame gamePlayerOrder
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromEncounterDeck c.id | c <- discarded]
      pushAll $ map OptionalActionWindow order <> [ResolveHideTest t {hideDiscarded = discarded}]
      pure attrs'
    ResetToStage _ rest -> pure $ attrs & questDeckL .~ rest
    MoveTopOfEncounterDeckToBottom -> case attrs.encounterDeck of
      top : rest -> pure $ attrs & encounterDeckL .~ rest <> [top]
      [] -> pure attrs
    GuardObjective oid -> do
      objectiveDef <- getsGame (fmap toCardDef . Map.lookup oid . entitiesObjectives . gameEntities)
      case [ts | Just d <- [objectiveDef], GuardedBy ts <- toList (cdKeywords d)] of
        -- Guarded (X): cards are discarded until one of those types is, which the
        -- objective reads back to guard it (MEC59 rules)
        types : _ -> do
          let matching c = cdCardType (toCardDef c) `elem` types
          push
            $ DiscardTopOfEncounterDeck (ObjectiveSource oid) (ObjectiveTarget oid)
            $ maybe (length attrs.encounterDeck) (+ 1) (findIndex matching attrs.encounterDeck)
          pure attrs
        [] -> case attrs.encounterDeck of
          card : rest -> do
            let def = toCardDef card
            push
              $ if
                -- it guards before it resolves, so before its surge; a treachery only until its
                -- effects resolve or are canceled
                | cdCardType def `elem` [EnemyType, LocationType, TreacheryType] ->
                    Run [SetObjectiveGuard oid card.id, RevealCard card]
                -- the next card guards this objective, then the revealed one is guarded in turn
                | Guarded `member` cdKeywords def -> Run [GuardObjective oid, RevealCard card]
                | otherwise -> RevealCard card
            pure $ attrs & encounterDeckL .~ rest
          []
            | null attrs.encounterDiscard -> pure attrs
            | otherwise -> do
                deck <- shuffleM attrs.encounterDiscard
                push $ GuardObjective oid
                pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
    -- Guarded (X) on a player attachment: the first card of its types discarded goes to
    -- the staging area guarding it; the discard pile is shuffled in if the deck runs out
    -- before one is (MEC65 rules), and with none anywhere it is free at once
    ResolveGuardedPlayerCard pid card types -> do
      let matching c = cdCardType (toCardDef c) `elem` types
      case break matching attrs.encounterDeck of
        (before, found : after) -> do
          pushAll
            $ [AddToEncounterDiscard before | notNull before]
            <> [CheckWindows [mkAfter $ DiscardedFromEncounterDeck c.id | c <- before <> [found]]]
            <> [GuardPlayerCard found.id AttachWhenFree card, AddToStaging found]
          pure $ attrs & encounterDeckL .~ after
        (_, [])
          | any matching attrs.encounterDiscard -> do
              deck <- shuffleM (attrs.encounterDiscard <> attrs.encounterDeck)
              push msg
              pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
          | otherwise -> do
              push $ AttachFreedCard pid card
              pure attrs
    GuardPlayerCard cid release card -> pure $ attrs & guardedPlayerCardsL %~ Map.insertWith (flip (<>)) cid [(release, card)]
    TakeGuardedPlayerCard cid card ->
      pure $ attrs & guardedPlayerCardsL %~ Map.filter notNull . Map.adjust (filter ((/= card) . (.id) . snd)) cid
    ReleaseGuardedPlayerCards cid -> do
      lead <- getLead
      pushAll
        [ case release of
            AttachWhenFree -> AttachFreedCard (fromMaybe lead card.owner) card
            ReturnToHandWhenFree -> PutInHand card
        | (release, card) <- Map.findWithDefault [] cid attrs.scenarioGuardedPlayerCards
        ]
      pure $ attrs & guardedPlayerCardsL %~ Map.delete cid
    ShuffleIntoCavesDeck card -> do
      deck <- shuffleM (card : filter (/= card) attrs.cavesDeck)
      pure $ attrs & cavesDeckL .~ deck & setAsideL %~ filter (/= card)
    PutOnBottomOfCavesDeck card -> pure $ attrs & cavesDeckL %~ ((<> [card]) . filter (/= card)) & setAsideL %~ filter (/= card)
    TakeFromCavesDeck card -> pure $ attrs & cavesDeckL %~ filter (/= card)
    ShuffleCavesDeck -> do
      deck <- shuffleM attrs.cavesDeck
      pure $ attrs & cavesDeckL .~ deck
    CreatePowerOfMordorDeck -> do
      deck <- shuffleM =<< gatherEncounterSet attrs ThePowerOfMordor
      pure $ attrs & powerOfMordorDeckL .~ deck
    -- revealed as the top card of the encounter deck is: an enemy or location to the
    -- staging area, an objective into play, a When Revealed resolved
    RevealTopOfPowerOfMordorDeck -> case attrs.powerOfMordorDeck of
      top : rest -> do
        push $ RevealCard top
        pure $ attrs & powerOfMordorDeckL .~ rest
      [] -> pure attrs
    CreateBurdenDeck cards -> do
      deck <- shuffleM (cards <> attrs.burdenDeck)
      pure $ attrs & burdenDeckL .~ deck
    RevealTopOfBurdenDeck -> case attrs.burdenDeck of
      top : rest -> do
        push $ RevealCard top
        pure $ attrs & burdenDeckL .~ rest
      [] -> pure attrs
    ShuffleTopOfBurdenDeckIntoEncounterDeck -> case attrs.burdenDeck of
      top : rest -> do
        deck <- shuffleM (top : attrs.encounterDeck)
        pure $ attrs & burdenDeckL .~ rest & encounterDeckL .~ deck
      [] -> pure attrs
    PlaceTopOfBurdenDeckOnEncounterDeck -> case attrs.burdenDeck of
      top : rest -> pure $ attrs & burdenDeckL .~ rest & encounterDeckL %~ (top :)
      [] -> pure attrs
    PlaceTopOfPowerOfMordorDeckUnderneath target -> case attrs.powerOfMordorDeck of
      top : rest -> do
        push $ PlaceUnderneath target [EncounterCard top]
        pure $ attrs & powerOfMordorDeckL .~ rest
      [] -> pure attrs
    -- the cards are drawn and resolved one at a time; with the deck empty the rest is
    -- ignored (We Must Away, Ere Break of Day rules)
    DrawFromSackDeck n | n > 0 -> case attrs.sackDeck of
      top : rest -> do
        pushAll $ DrawSackCard top : [DrawFromSackDeck (n - 1) | n > 1]
        pure $ attrs & sackDeckL .~ rest
      [] -> pure attrs
    ShuffleIntoSackDeck card -> do
      deck <- shuffleM (card : filter (/= card) attrs.sackDeck)
      pure $ attrs & sackDeckL .~ deck
    -- the Evil Creatures discard pile is shuffled back in once the deck is empty, as the
    -- last card leaves it (Journey Up the Anduin rules)
    AddFromEvilCreaturesDeck source engaging -> case attrs.evilCreaturesDeck of
      Just (top : rest) -> do
        push $ maybe (AddToStaging top) (`PutEnemyIntoPlayEngaged` top) engaging
        push $ AddedFromEvilCreaturesDeck source top
        if null rest
          then do
            deck <- shuffleM attrs.scenarioEvilCreaturesDiscard
            pure $ attrs & evilCreaturesDeckL ?~ deck & evilCreaturesDiscardL .~ []
          else pure $ attrs & evilCreaturesDeckL ?~ rest
      _ -> pure attrs
    -- Guarded (X) on a location, as Lost in Mirkwood's objective-locations: the first card
    -- of its types discarded goes to the staging area, the location attached to it; the
    -- discard pile is shuffled in should the deck run out first (MEC65 rules)
    GuardLocationBy lid types -> do
      let matching c = cdCardType (toCardDef c) `elem` types
      case break matching attrs.encounterDeck of
        (before, found : after) -> do
          pushAll
            $ [AddToEncounterDiscard before | notNull before]
            <> [CheckWindows [mkAfter $ DiscardedFromEncounterDeck c.id | c <- before <> [found]]]
            <> [AddToStaging found, PlaceLocation lid (AttachedTo $ CardIdTarget found.id)]
          pure $ attrs & encounterDeckL .~ after
        (_, [])
          | any matching attrs.encounterDiscard -> do
              deck <- shuffleM (attrs.encounterDiscard <> attrs.encounterDeck)
              push msg
              pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
          | otherwise -> pure attrs
    -- the next encounter card guards the location, attached to it as it is revealed so its
    -- own effects see it guarding (Troll-fells); any other card is only revealed
    GuardLocation lid -> case attrs.encounterDeck of
      card : rest -> do
        push
          $ if cdCardType (toCardDef card) `elem` [EnemyType, LocationType, TreacheryType, EncounterSideQuestType]
            then Run [PlaceLocation lid (AttachedTo $ CardIdTarget card.id), RevealCard card]
            else RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      []
        | null attrs.encounterDiscard -> pure attrs
        | otherwise -> do
            deck <- shuffleM attrs.encounterDiscard
            push $ GuardLocation lid
            pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
    AddSetAsideToStaging matcher -> do
      let (matching, rest) = partition (cardMatches matcher) attrs.scenarioSetAside
      traverse_ (push . AddToStaging) matching
      pure $ attrs & setAsideL .~ rest
    ShuffleEncounterDiscardIntoDeck matcher -> do
      let (matching, rest) = partition (cardMatches matcher) attrs.encounterDiscard
      deck <- shuffleM (matching <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ rest
    SetAside card -> pure $ attrs & setAsideL %~ (card :) & encounterDeckL %~ filter (/= card)
    PlaceResourcesOutOfPlay cid n -> pure $ attrs & outOfPlayResourcesL %~ Map.insertWith (+) cid n
    -- a set-aside card revealed, as Cornered Animals reveals Orc Ambush
    RevealCard card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside card added to the staging area, as Orc Trail adds one out-of-play Orc
    AddToStaging card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- or made an enemy where its card puts it, as Racing North adds Uruk Warg-rider to the
    -- Orcs' area
    CreateEnemy _ card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside card put into play, as The Seething Lake makes Doors of Durin active
    PutIntoPlayAsActiveLocation card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside objective attached to an encounter card, as Defeat the Serpent attaches
    -- one to The Black Serpent
    PutObjectiveIntoPlayGuardedBy card _ -> pure $ attrs & setAsideL %~ filter (/= card)
    ShuffleSetAsideIntoEncounterDeck cards -> do
      let shuffled = filter (`elem` cards) attrs.scenarioSetAside
      deck <- shuffleM (shuffled <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & setAsideL %~ filter (`notElem` shuffled)
    ShuffleVictoryDisplayIntoEncounterDeck matcher -> do
      let (matching, rest) = partition (\case EncounterCard c -> cardMatches matcher c; _ -> False) attrs.victoryDisplay
      deck <- shuffleM ([c | EncounterCard c <- matching] <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & victoryDisplayL .~ rest
    PutOnTopOfEncounterDeck card -> pure $ attrs & encounterDeckL %~ (card :)
    -- a player card with the Encounter keyword is treated like an encounter card, as Wind
    -- from the Sea or Eagle of the North after it was a shadow card (MEC61, MEC67 rules),
    -- but Ranger of the North goes to its owner's discard pile instead
    AddToEncounterDiscard cards -> do
      let asEncounterCard c = Encounter `member` cdKeywords (toCardDef c) && not (cdEncounterToOwnersDiscard $ toCardDef c)
      owned <- for cards \c -> (c,) <$> if asEncounterCard c then pure Nothing else setAsideOwner c.id
      pushAll [PutInDiscard (MkPlayerCard c.id (toCardCode c) (Just owner)) | (c, Just owner) <- owned]
      -- a card of the Corsair deck goes to its own discard pile, while there is one; a
      -- set-aside card discarded is part of the encounter deck from then on (MEC61 rules)
      pure $ foldr discardEncounterCard attrs [c | (c, Nothing) <- owned] & setAsideL %~ filter (`notElem` cards)
    RemoveSetAsideFromGame cards -> pure $ attrs & setAsideL %~ filter (`notElem` cards)
    -- a set-aside card with the Encounter keyword, as Ranger Summons shuffles one in, or a
    -- card in the encounter discard pile, as Calaerion shuffles one back
    ShuffleIntoEncounterDeck (CardIdTarget cid) -> do
      owned <- getsGame (concatMap (.setAsideCards) . toList . gamePlayers)
      let (discarded, rest) = partition ((== cid) . (.id)) attrs.encounterDiscard
      deck <- shuffleM $ [MkEncounterCard cid (toCardCode c) | c <- owned, c.id == cid] <> discarded <> attrs.encounterDeck
      pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ rest
    RemoveFromEncounterDeck card -> pure $ attrs & encounterDeckL %~ filter (/= card)
    RemoveFromEncounterDiscard card -> pure $ attrs & encounterDiscardL %~ filter (/= card)
    RemoveFromVictoryDisplay card -> pure $ attrs & victoryDisplayL %~ filter ((/= card.id) . (.id))
    SearchEncounterDeckAndDiscard pid source matcher -> search False pid source matcher AddToStaging
    SearchEncounterDeckAndDiscardToEngage pid source matcher -> search False pid source matcher (PutEnemyIntoPlayEngaged pid)
    SearchEncounterDeckAndDiscardToReveal pid source matcher -> search False pid source matcher RevealCard
    SearchEncounterDeckAndDiscardToRevealEngaged pid source matcher -> search False pid source matcher (RevealCardEngaged pid)
    SearchEncounterCardsAndVictoryDisplay pid source matcher -> search True pid source matcher AddToStaging
    -- a set-aside enemy put into play engaged, as No Way Out puts Chief Turch
    PutEnemyIntoPlayEngaged _ card -> pure $ attrs & setAsideL %~ filter (/= card)
    PutObjectiveIntoPlayAttached card _ -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside objective put into play free of encounters, as a stage 2A of Roam Across
    -- Rhovanion puts Durin's Key with no Deep Ravine to guard it
    PutObjectiveIntoPlay card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside Ship objective put into play, as The Beacon gives a Dol Amroth Warship
    PutObjectiveAllyIntoPlayUnder _ card -> pure $ attrs & setAsideL %~ filter (/= card)
    PutObjectiveHeroIntoPlayUnder _ card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside card removed from the game, as the objective-heroes no player chose
    RemoveFromGame (CardIdTarget cid) -> pure $ attrs & setAsideL %~ filter ((/= cid) . (.id))
    GuardCard (EncounterCard card) _ -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside card placed under a card, as a Warden under a Camp (The Land of Sorrow)
    PlaceUnderneath _ cards -> pure $ attrs & setAsideL %~ filter ((`notElem` map (.id) cards) . (.id))
    DebugSearchEncounterDeck pid -> do
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.revealCard") Nothing []
        $ SearchCards (map EncounterCard attrs.encounterDeck)
        $ ChooseOne
        $ Label (ikey "label.done") []
        : [targetLabel y [RemoveFromEncounterDeck y, RevealCard y] | y <- attrs.encounterDeck]
      pure attrs
    AdvanceQuest -> case attrs.questDeck of
      [] -> do
        push $ GameOver Won
        pure attrs
      (next : rest) -> do
        push $ AdvanceToQuest next
        pure $ attrs & questDeckL .~ rest
    ShuffleIntoQuestDeck card -> do
      deck <- shuffleM (card : filter (/= card) attrs.questDeck)
      pure $ attrs & questDeckL .~ deck & victoryDisplayL %~ filter ((/= card.id) . (.id))
    RemoveFromQuestDeck card -> pure $ attrs & questDeckL %~ filter (/= card)
    PutOnBottomOfQuestDeck card ->
      pure $ attrs & questDeckL %~ ((<> [card]) . filter (/= card)) & victoryDisplayL %~ filter ((/= card.id) . (.id))
    -- a stage put into play straight from the quest deck, as Hasty Council chooses one, leaves it
    PutQuestIntoPlay card -> pure $ attrs & questDeckL %~ filter (/= card)
    AdvanceToQuest card -> do
      pushAll [PutQuestIntoPlay card, Flip (toTarget card), CheckWindows [mkAfter AdvancedToStage]]
      pure attrs
    TakeHiddenCard pid -> do
      (deck, discard) <-
        if null attrs.encounterDeck
          then (,[]) <$> shuffleM attrs.encounterDiscard
          else pure (attrs.encounterDeck, attrs.encounterDiscard)
      pure $ case deck of
        card : rest -> attrs & encounterDeckL .~ rest & encounterDiscardL .~ discard & hiddenCardsL %~ Map.insertWith (flip (<>)) pid [card]
        [] -> attrs
    TurnHiddenCardFaceup pid card msgs | card `elem` hiddenOf pid -> do
      pushAll msgs
      pure $ attrs & hiddenCardsL . ix pid %~ filter (/= card)
    DiscardHiddenCard pid card | card `elem` hiddenOf pid ->
      pure $ attrs & hiddenCardsL . ix pid %~ filter (/= card) & encounterDiscardL %~ (card :)
    -- an eliminated player's hidden cards are discarded
    EliminatePlayer pid -> pure $ attrs & hiddenCardsL %~ Map.delete pid & encounterDiscardL %~ (hiddenOf pid <>)
    _ -> runSailing msg attrs
   where
    -- the deck reset as it empties partway, the rest are discarded from it (ruling, Collateral
    -- Damage)
    discardFrom k a acc = do
      let (top, rest) = splitAt k a.encounterDeck
          a' = a & encounterDeckL .~ rest & encounterDiscardL %~ (top <>)
          left = k - length top
      a'' <- resetEmptyEncounterDeck a'
      if left > 0 && notNull a''.encounterDeck
        then discardFrom left a'' (acc <> top)
        else pure (a'', acc <> top)
    -- "If the encounter deck is ever empty during the quest phase, the encounter discard
    -- pile is shuffled and reset back into the encounter deck."
    resetEmptyEncounterDeck a = do
      phase <- getsGame gamePhase
      gameMods <- getModifiers GameTarget
      let anyPhase = EncounterDeckResetsWheneverEmpty `elem` gameMods
          kept = EncounterDeckDoesNotReset `elem` gameMods
      if (phase == QuestPhase || anyPhase) && not kept && null a.encounterDeck && notNull a.encounterDiscard
        then do
          -- the topmost matching card goes to the staging area first, as Ghostly Ruins reads
          let matchers = [m | ReturnBeforeEncounterDeckResets m <- gameMods]
          let returned = take 1 [c | c <- a.encounterDiscard, any (`cardMatches` c) matchers]
          traverse_ (push . AddToStaging) returned
          deck <- shuffleM $ filter (`notElem` returned) a.encounterDiscard
          pure $ a & encounterDeckL .~ deck & encounterDiscardL .~ []
        else pure a
    hiddenOf pid = Map.findWithDefault [] pid attrs.hiddenCards
    -- the player searches the encounter deck and discard pile, the card found going where
    -- the message puts it, then the deck is shuffled
    search withVictory pid source matcher put = do
      let fromDeck = filter (cardMatches matcher) attrs.encounterDeck
          fromDiscard = filter (cardMatches matcher) attrs.encounterDiscard
          victory = if withVictory then [c | EncounterCard c <- attrs.victoryDisplay] else []
          fromVictory = filter (cardMatches matcher) victory
          take' remove y = targetLabel y [remove y, put y, ShuffleDeck EncounterDeck]
          choices =
            map (take' RemoveFromEncounterDeck) fromDeck
              <> map (take' RemoveFromEncounterDiscard) fromDiscard
              <> map (take' (RemoveFromVictoryDisplay . EncounterCard)) fromVictory
      push
        $ Ask pid
        $ QuestionLabel
          (if null choices then (ikey "label.searchFoundNothing") else (ikey "label.searchChoose"))
          (Just source)
          []
        $ SearchCards (map EncounterCard $ attrs.encounterDeck <> attrs.encounterDiscard <> victory)
        $ ChooseOne
        $ if null choices then [Label (ikey "label.continue") [ShuffleDeck EncounterDeck]] else choices
      pure attrs
