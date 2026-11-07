{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Player.Runner (module LordOfTheRings.Player.Runner, module X) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.ActiveCost (ActiveCost (..), markPlayed, playCard)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import LordOfTheRings.Ally (createAlly)
import LordOfTheRings.Ally.Types (AllyAttrs (allyHandAction))
import Data.List (elemIndex)
import LordOfTheRings.Decklist (DeckList (DeckList), deckProblem)
import LordOfTheRings.Campaign (seatThreatPenalty)
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Effect
import LordOfTheRings.Entities (entitiesTreacheries)
import LordOfTheRings.Event
import LordOfTheRings.Frontend (FrontendMessage (..), send)
import LordOfTheRings.Game.Base
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Helpers.Doomed (playerCardDoomed)
import LordOfTheRings.Helpers.Discard (movableDiscardCards)
import LordOfTheRings.Helpers.Modifiers (getModifiers, modifiedValue, playProhibitions, threatOf)
import LordOfTheRings.Helpers.Window (actionsAndResponsesAllowed, exhaustableCharacters, getActionAbilities, limitSource, passesCriteria, passesCriteriaFor, travelableLocations)
import LordOfTheRings.Hero.Types
import LordOfTheRings.I18n
import LordOfTheRings.Keyword (Keyword (GuardedBy, Secrecy))
import LordOfTheRings.Id (AllyId (..), PlayerId (..), CardId (..))
import LordOfTheRings.Log.Block (LogBlockKind (..))
import LordOfTheRings.Log.Refs (phaseKey)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (raiseThreatFromDoomed)
import LordOfTheRings.Modifier
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Player.Types as X
import LordOfTheRings.Prelude hiding (swap)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (scenarioCampaign)
import LordOfTheRings.Question
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Trait (Trait (Master))
import LordOfTheRings.Treachery.Types ()
import LordOfTheRings.Window (
  Window (Window),
  WindowType (CardsDrawn, CardsDrawnBy, DiscardedFromDeck, DrewForResourcePhase, SetupHandDrawn, DiscardedFromHand, EnemyAttacking, PlayedCard, PlayedCardFromHand, PlayedFromHand, ResourcesSpent, ThreatRaised),
  Timing (After),
  mkAfter,
  mkWhen,
 )

instance RunMessage Player where
  runMessage msg player = do
    locked <- elem DiscardPilesLocked <$> getModifiers GameTarget
    -- a deck that cannot be searched, as Soulless Cadaver's, gives up nothing to a search
    barred <- case msg of
      ChooseFromSearch pid _ _ _ _ | pid == player.id -> elem CannotSearchDeck <$> getModifiers (PlayerTarget pid)
      ChooseFromSearchIntoPlay pid _ _ _ _ _ | pid == player.id -> elem CannotSearchDeck <$> getModifiers (PlayerTarget pid)
      _ -> pure False
    player' <- if (locked && takesFromDiscard) || barred then pure player else runPlayer msg player
    -- a deck run out may eliminate its player, as Deadmen's Dike reads
    when (notNull player.deck && null player'.deck) $ push CheckEliminations
    pure player'
   where
    -- what a player card effect does to take a card out of the discard pile
    takesFromDiscard = case msg of
      ReturnFromDiscardToHand pid _ -> pid == player.id
      PutDiscardOnBottomOfDeck pid _ -> pid == player.id
      PutDiscardOnTopOfDeck pid _ -> pid == player.id
      ShuffleFromDiscardIntoDeck pid _ -> pid == player.id
      ShuffleDiscardIntoDeck pid -> pid == player.id
      _ -> False

-- | A random discard may report the cards actually discarded back to its source,
-- following Arkham's source-tagged DiscardedCards result messages.
randomDiscardRequest :: Message -> Maybe (PlayerId, Int, Maybe Source, Maybe Message)
randomDiscardRequest = \case
  DiscardRandomFromHand pid n -> Just (pid, n, Nothing, Nothing)
  DiscardRandomFromHandWithResult source pid n -> Just (pid, n, Just source, Nothing)
  DiscardRandomFromHandThen pid n follow -> Just (pid, n, Nothing, Just follow)
  _ -> Nothing

-- Cards held by another player still leave hand for their original owner's
-- discard pile. Keep the holder's discard windows and result messages unchanged.
discardToOwners :: Player -> [Card] -> QueueT Message GameT [Card]
discardToOwners player cards = do
  let foreignOwned = [pc | PlayerCard pc <- cards, Just owner <- [pc.owner], owner /= player.id]
  pushAll $ map PutInDiscard foreignOwned
  pure $ filter ((`notElem` map (.id) foreignOwned) . (.id)) cards

runPlayer :: Message -> Player -> QueueT Message GameT Player
runPlayer msg player = case msg of
    DoStep 1 Setup -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    DoStep 2 Setup -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
      -- a hero's threat cost may be lowered, as Mirlonde's
      threat <- sum <$> traverse (\h -> modifiedValue threatOf h h.threat) heroes
      -- in campaign mode, with the threat penalty the campaign log records
      seat <- getsGame (fromMaybe 0 . elemIndex player.id . gamePlayerOrder)
      penalty <- getsGame (maybe 0 (seatThreatPenalty seat) . scenarioCampaign . toAttrs . gameScenario)
      pure $ player & threatL .~ threat + penalty & startingThreatL .~ threat + penalty
    DoStep 5 Setup -> do
      cannotMulligan <- elem CannotMulligan <$> getModifiers (PlayerTarget player.id)
      -- a response instead of a mulligan, as Galdor of the Havens', takes the question away
      push $ CheckWindows [mkAfter $ SetupHandDrawn player.id]
      push
        $ questionLabel
          (ikey "label.keepStartingHand")
          player.id
          ([Label (ikey "label.keepHand") []] <> [Label (ikey "label.mulligan") [Mulligan player.id] | not cannotMulligan])
      -- a card a hero's Setup put in hand stays there, as Thurindir's side quest (Caleb's
      -- ruling: it is searched for before the setup hand, and kept through a mulligan)
      size <- getStartingHandSizeFor player.id
      let (hand, deck) = splitAt size player.deck
      pure $ player & handL <>~ hand & deckL .~ deck
    DiscardEventsFromHand pid | pid == player.id -> do
      let (events, rest) = partition ((== EventType) . (.cardType)) player.hand
      ownDiscard <- discardToOwners player events
      pure $ player & handL .~ rest & discardL %~ (ownDiscard <>)
    Mulligan pid | pid == player.id -> do
      -- only the setup hand, the last cards in hand, goes back
      size <- getStartingHandSizeFor player.id
      let (kept, drawn) = splitAt (length player.hand - size) player.hand
      send $ ShuffleIntoDeck $ map (.id) drawn
      (hand, deck) <- splitAt size <$> shuffleM (drawn <> player.deck)
      pure $ player & handL .~ kept <> hand & deckL .~ deck
    -- a raise opens a window before it, as Harbor Thug's Forced effect answers
    RaiseThreat pid n | pid == player.id, n > 0 -> do
      pushAll [CheckWindows [mkWhen $ ThreatRaised pid], DoRaiseThreat pid n]
      pure player
    RaiseThreat pid n | pid == player.id -> do
      push CheckEliminations
      pure $ player & threatL %~ max 0 . (+ n)
    AddToStartingThreat pid n | pid == player.id -> do
      push CheckEliminations
      pure $ player & threatL %~ max 0 . (+ n) & startingThreatL %~ max 0 . (+ n)
    DoRaiseThreat pid n | pid == player.id -> do
      pushAll
        [ CreateEffect $ Effect GameSource (PlayerTarget pid) [RaisedThreatThisRound] UntilEndOfRound
        , CheckEliminations
        , CheckWindows [mkAfter $ ThreatRaised pid]
        ]
      pure $ player & threatL %~ max 0 . (+ n)
    EliminatePlayer pid | pid == player.id -> do
      level <- getEliminationLevel pid
      ownDiscard <- discardToOwners player player.hand
      pure $ player & threatL .~ level & eliminatedL .~ True & discardL %~ (ownDiscard <>) & handL .~ []
    DoPhase ResourcePhase -> do
      mods <- getModifiers player
      let n = fromMaybe 1 $ headMay [k | ResourcePhaseDraws k <- mods]
          -- an additional card is part of the same draw, none when there is none
          extra = sum [k | AdditionalResourcePhaseDraws k <- mods]
      pushWhen (n > 0) $ DrawCards $ Drawing player.id (n + extra)
      -- the cards that draw takes, for what answers them, as Círdan the Shipwright: those
      -- still in hand after it
      pushWhen (n > 0) $ CheckWindows [mkAfter $ DrewForResourcePhase player.id (map (.id) $ take (n + extra) player.deck)]
      pure player
    DrawCards drawing | drawing.player == player.id -> do
      blocked <- elem CannotDrawCards <$> getModifiers player
      -- a condition on top of the deck is discarded in place of the next draw
      shackles <-
        getsGame
          ( filter ((== AttachedTo (toTarget player)) . (.placement))
              . toList
              . entitiesTreacheries
              . gameEntities
          )
      pushAll [Discard (toSource t) (toTarget t) | t <- take 1 shackles]
      unless (blocked || notNull shackles) $ push $ DoDrawCards drawing
      pure player
    DoDrawCards drawing | drawing.player == player.id -> do
      let (cards, deck) = splitAt drawing.count player.deck
          -- a hero in the deck was shuffled in by Lost and Alone, and is put into play as drawn
          (heroes, drawn) = partition ((== HeroType) . (.cardType)) cards
      pushAll [PutHeroIntoPlay player.id pc | PlayerCard pc <- heroes]
      pushAll $ concat [[EncounterCardEnteredHand player.id ec, EncounterCardDrawn player.id ec] | EncounterCard ec <- drawn]
      pushWhen (notNull cards) $ CheckWindows [mkAfter $ CardsDrawn player.id (length cards), mkAfter $ CardsDrawnBy player.id drawing.source]
      pure $ player & handL <>~ drawn & deckL .~ deck
    WithTarget (isTarget player -> True) (DoPhase PlanningPhase) -> do
      actionWindow (ikey "label.planningActions") (ikey "label.donePlanning") (Just QuestPhase)
        $ WithTarget (toTarget player) (DoPhase PlanningPhase)
      pure player
    ActionWindow pid | pid == player.id -> do
      -- travel's window is the last thing in its phase unless the lead may travel after it;
      -- the others come before more of theirs
      travel <- isPhase TravelPhase
      last' <- if travel then null <$> travelableLocations else pure False
      actionWindow (ikey "label.takeAction") (ikey "label.pass") (guard last' $> EncounterPhase)
        $ ActionWindow pid
      pure player
    OptionalActionWindow pid | pid == player.id -> do
      playable <- playableCards player
      actions <- getActionAbilities player.id
      fromHand <- handActions player
      unless (null playable && null actions && null fromHand)
        $ actionWindow (ikey "label.takeAction") (ikey "label.pass") Nothing
        $ OptionalActionWindow pid
      pure player
    PlayCard pid cid | pid == player.id -> do
      push $ PlayCardInWindow pid cid []
      pure player
    -- from hand, or from the discard pile for a card that may be played from there
    PlayCardInWindow pid cid windows | pid == player.id -> do
      fromTop <- topOfDeckPlayers player
      let deckTop = [c | not (null fromTop), c <- take 1 player.deck]
      card <- maybe (error "not able to find card") pure $ find ((== cid) . (.id)) (player.hand <> player.discard <> deckTop)
      let fromDeck = cid `elem` map (.id) deckTop
          -- played from the top of the deck as if it was in hand, as Gandalf lets it be
          fromHand = cid `elem` map (.id) player.hand || fromDeck
          ownDiscard = cdPlayableFrom (toCardDef card) `elem` [FromHandOrDiscard, FromDiscardOnly]
      -- the card that let it be played has been used for the phase
      pushAll [CreateEffect $ Effect source t [PlayedTopOfDeckThisPhase cid] UntilEndOfPhase | fromDeck, source <- fromTop, Just t <- [sourceTarget source]]
      -- a card in the discard pile a Record lets be played: the Record is discarded for it,
      -- and the card goes to the bottom of the deck after
      records <- if fromHand || ownDiscard then pure [] else discardPlayers pid card
      -- what it costs from hand besides its resources is paid first, as Guardian of
      -- Rivendell's discards
      let additional = [ChooseAndDiscardFromHand pid (CardIdSource cid) n | fromHand, Just (DiscardCardsFromHand n) <- [cdAdditionalCost (toCardDef card)]]
      played <- inLogBlock (PlayBlock pid card) (additional <> [PayForCard pid card fromHand windows])
      push $ case records of
        [] -> Run played
        _ -> chooseOrRunOne pid [targetLabel r (Discard GameSource r : played <> [PutDiscardOnBottomOfDeck pid cid]) | r <- records]
      pure $ player & handL %~ filter ((/= cid) . (.id)) & discardL %~ filter ((/= cid) . (.id)) & deckL %~ filter ((/= cid) . (.id))
    PutAllyIntoPlay pid cid | pid == player.id -> do
      takeFromHand cid >>= \case
        PlayerCard card -> do
          -- put into play, a card with Doomed raises each player's threat as one played does
          raiseThreatFromDoomed =<< playerCardDoomed pid (PlayerCard card)
          push $ PlayAlly pid card
        EncounterCard _ -> error "not an ally"
      pure $ player & handL %~ filter ((/= cid) . (.id))
    -- attached without being played, as Plundered Armoury lets one be
    PutAttachmentIntoPlayFromHand pid cid host | pid == player.id -> do
      takeFromHand cid >>= \case
        PlayerCard card -> push $ PlayAttachmentOn pid card host
        EncounterCard _ -> error "not an attachment"
      pure $ player & handL %~ filter ((/= cid) . (.id))
    PlayEvent pid card _ | pid == player.id -> do
      ownDiscard <- discardToOwners player [PlayerCard card]
      pure $ player & discardL %~ (ownDiscard <>)
    -- an event whose cost is X, played for no cost, has X of 0 and so does nothing, as
    -- Men of the West played with Vilya (ruling)
    PlayAtNoCost pid card
      | pid == player.id
      , cdCardType (toCardDef card) == EventType
      , cdCost (toCardDef card) == Just DynamicCost -> do
          pushAll $ markPlayed pid card <> [CheckWindows [mkAfter $ PlayedCard pid card]]
          ownDiscard <- discardToOwners player [PlayerCard card]
          pure $ player & discardL %~ (ownDiscard <>)
    PlayAtNoCost pid card | pid == player.id -> do
      -- played from wherever, it counts as one paid for does, as Silent Guardian reads
      pushAll $ markPlayed pid card <> playCard pid card [] <> [CheckWindows [mkAfter $ PlayedCard pid card]]
      pure player
    PlayFromHandAtNoCost pid cid | pid == player.id -> case find ((== cid) . (.id)) player.hand of
      Just card@(PlayerCard pc) -> do
        pushAll
          =<< inLogBlock
            (PlayBlock pid card)
            [ PlayAtNoCost pid pc
            , CheckWindows
                [mkAfter $ PlayedFromHand (toTarget pc), mkAfter $ PlayedCardFromHand pid (toCardCode pc)]
            ]
        pure $ player & handL %~ filter ((/= cid) . (.id))
      _ -> pure player
    -- a Guarded (X) attachment is guarded first, as one played is (MEC65 rules)
    PlayFromHandOnAtNoCost pid cid host | pid == player.id -> case find ((== cid) . (.id)) player.hand of
      Just card@(PlayerCard pc) -> do
        eligible <- elem host <$> playableCardHosts pid (toCardCode pc)
        if not eligible
          then pure player
          else do
            raiseThreatFromDoomed =<< playerCardDoomed pid (PlayerCard pc)
            let attach = maybe (PlayAttachmentOn pid pc host) (ResolveGuardedPlayerCard pid pc) $ headMay [ts | GuardedBy ts <- toList (cdKeywords $ toCardDef pc)]
            pushAll
              =<< inLogBlock
                (PlayBlock pid card)
                ( markPlayed pid pc
                    <> [ attach
                       , CheckWindows
                           [mkAfter $ PlayedCard pid pc, mkAfter $ PlayedFromHand (toTarget pc), mkAfter $ PlayedCardFromHand pid (toCardCode pc)]
                       ]
                )
            pure $ player & handL %~ filter ((/= cid) . (.id))
      _ -> pure player
    PlayTopOfDeckAtNoCostOrBottom pid source | pid == player.id -> do
      for_ (headMay player.deck) \case
        top@(PlayerCard pc) -> do
          able <- canPlayAtNoCost pid top
          -- an ally or attachment is put into play, not played: only an event is played
          -- (ruling, Vilya)
          let putIntoPlay = top.cardType == AllyType || isAttachmentType top.cardType
              resolve = case top.cardType of
                AllyType -> [PutAllyIntoPlayUnder pid pc]
                t | isAttachmentType t -> [PutAttachmentIntoPlay pid pc]
                _ -> [PlayAtNoCost pid pc]
          push
            $ Ask pid
            $ QuestionLabel (ikey "label.playAtNoCost") (sourceTarget source) []
            $ SearchCards [top]
            $ ChooseOne
            $ [Label (ikey $ if putIntoPlay then "label.putItIntoPlay" else "label.playIt") (RemoveFromDeck pid pc.id : resolve) | able]
            <> [Label (ikey "label.moveToBottomOfDeck") [MoveToBottomOfDeck pid pc.id]]
        EncounterCard _ -> pure ()
      pure player
    RemoveFromDeck pid cid | pid == player.id -> pure $ player & deckL %~ filter ((/= cid) . (.id))
    MoveToBottomOfDeck pid cid | pid == player.id -> do
      let (moved, kept) = partition ((== cid) . (.id)) player.deck
      pure $ player & deckL .~ kept <> moved
    MoveToTopOfDeck pid cid | pid == player.id -> do
      let (moved, kept) = partition ((== cid) . (.id)) player.deck
      pure $ player & deckL .~ moved <> kept
    RemoveFromDiscard pid cid | pid == player.id -> pure $ player & discardL %~ filter ((/= cid) . (.id))
    -- of the attachments it may go on, those the player can afford to play there
    PlayAttachmentFromDiscardOn pid source host | pid == player.id -> do
      playable <- flip filterM [pc | PlayerCard pc <- player.discard, cdCardType (toCardDef pc) == AttachmentType] \pc -> do
        let card = PlayerCard pc
        discount <- attachDiscount card host
        andM [elem host <$> playableCardHosts pid (toCardCode pc), passesUnique card, not <$> cannotPlay pid card, canAffordWith discount player card]
      choices <- for playable \pc ->
        targetLabel (CardIdTarget pc.id)
          <$> inLogBlock (PlayBlock pid (PlayerCard pc)) [RemoveFromDiscard pid pc.id, PayToAttach pid (PlayerCard pc) False [] host]
      pushWhen (notNull choices)
        $ Ask pid
        $ QuestionLabel (ikey "label.playAttachmentFromDiscard") (sourceTarget source) [host]
        $ SearchCards (map PlayerCard playable)
        $ ChooseOne choices
      pure player
    -- a card placed underneath another leaves wherever its owner had it
    PlaceUnderneath _ cards -> do
      let gone c = c.id `elem` map (.id) cards
      pure $ player & handL %~ filter (not . gone) & deckL %~ filter (not . gone) & discardL %~ filter (not . gone)
    ShuffleDiscardIntoDeck pid | pid == player.id -> do
      moved <- movableDiscardCards player.discard
      deck <- shuffleM (player.deck <> moved)
      pure $ player & deckL .~ deck & discardL %~ filter (`notElem` moved)
    PlaceUnderThreatDial pid n | pid == player.id -> do
      let (placed, rest) = splitAt n player.deck
      pure $ player & deckL .~ rest & underThreatDialL <>~ placed
    DiscardFromUnderThreatDial pid | pid == player.id -> case player.underThreatDial of
      top : rest -> pure $ player & underThreatDialL .~ rest & discardL %~ (top :)
      [] -> pure player
    ShuffleUnderThreatDialIntoDeck pid | pid == player.id -> do
      deck <- shuffleM (player.deck <> player.underThreatDial)
      pure $ player & deckL .~ deck & underThreatDialL .~ []
    TakeFromDeck pid cid | pid == player.id -> do
      let (taken, kept) = partition ((== cid) . (.id)) player.deck
      pushAll [EncounterCardEnteredHand pid ec | EncounterCard ec <- taken]
      pure $ player & deckL .~ kept & handL <>~ taken
    DebugSearchDeck pid | pid == player.id -> do
      -- not shuffled after: a debug search leaves the rest of the deck in its order
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.takeCardIntoHand") Nothing []
        $ SearchCards player.deck
        $ ChooseOne
        $ Label (ikey "label.done") []
        : [targetLabel (CardIdTarget c.id) [TakeFromDeck pid c.id, DebugSearchDeck pid] | c <- player.deck]
      pure player
    ReturnFromDiscardToHand pid cid | pid == player.id -> do
      let (returned, kept) = partition ((== cid) . (.id)) player.discard
      pure $ player & discardL .~ kept & handL <>~ returned
    PlaceOrcGuard pid | pid == player.id -> case player.deck of
      PlayerCard top : rest -> do
        push $ EngageOrcGuard pid top
        pure $ player & deckL .~ rest
      _ -> pure player
    ChooseFromSearch pid source cards matcher anyNumber | pid == player.id -> do
      let inDeck = filter ((`elem` map (.id) player.deck) . (.id)) cards
          again c = [ChooseFromSearch pid source (filter ((/= c.id) . (.id)) inDeck) matcher True | anyNumber]
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.searchTake") (sourceTarget source) []
        $ SearchCards inDeck
        $ ChooseOne
        $ [ targetLabel
              (CardIdTarget c.id)
              (TakeFromDeck pid c.id : again c <> [ShufflePlayerDeck pid | not anyNumber])
          | c <- inDeck
          , cardMatches matcher c
          ]
        <> [Label (ikey "label.done") [ShufflePlayerDeck pid]]
      pure player
    ChooseFromSearchIntoPlay pid source cards matcher n mods | pid == player.id && n > 0 -> do
      let inDeck = filter ((`elem` map (.id) player.deck) . (.id)) cards
      allies <- filterM passesUnique =<< filterM (matchCard matcher) [c | c <- inDeck, c.cardType == AllyType]
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.searchPutIntoPlay") (sourceTarget source) []
        $ SearchCards inDeck
        $ ChooseOne
        $ [ targetLabel (CardIdTarget c.id)
              $ [CreateEffect $ Effect source (CardIdTarget c.id) mods UntilEndOfRound | notNull mods]
              <> [ TakeFromDeck pid c.id
                 , PutAllyIntoPlay pid c.id
                 , ChooseFromSearchIntoPlay pid source (filter ((/= c.id) . (.id)) inDeck) matcher (n - 1) mods
                 ]
          | c <- allies
          ]
        <> [Label (ikey "label.done") [ShufflePlayerDeck pid]]
      pure player
    ChooseFromSearchIntoPlay pid _ _ _ _ _ | pid == player.id -> do
      push $ ShufflePlayerDeck pid
      pure player
    ResolvePillage eid pid | pid == player.id -> do
      alive <- selectAny $ EnemyWithId eid
      n <- if alive then getPillage eid else pure 0
      let discarded = take n player.deck
      when (notNull discarded) do
        push $ DiscardTopOfDeck pid n
        push $ Pillaged eid pid discarded
      pure player
    DiscardTopOfDeck pid n | pid == player.id -> do
      let (discarded, deck) = splitAt n player.deck
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromDeck pid c.id | c <- discarded]
      -- an encounter card in the deck goes to the encounter discard pile, unless it
      -- functions like a player card
      let (own, encounter) = partition functionsLikeAPlayerCard discarded
      pushWhen (notNull encounter) $ AddToEncounterDiscard [ec | EncounterCard ec <- encounter]
      pure $ player & deckL .~ deck & discardL %~ (own <>)
    SwapHandWithDeck pid fromHand fromDeck | pid == player.id -> do
      let swap c
            | c.id == fromHand = fromMaybe c $ find ((== fromDeck) . (.id)) player.deck
            | c.id == fromDeck = fromMaybe c $ find ((== fromHand) . (.id)) player.hand
            | otherwise = c
      pure $ player & handL %~ map swap & deckL %~ map swap
    PutDiscardOnBottomOfDeck pid cid | pid == player.id -> do
      let (moved, kept) = partition ((== cid) . (.id)) player.discard
      pure $ player & discardL .~ kept & deckL <>~ moved
    PutDiscardOnTopOfDeck pid cid | pid == player.id -> do
      let (moved, kept) = partition ((== cid) . (.id)) player.discard
      pure $ player & discardL .~ kept & deckL %~ (moved <>)
    PutOnTopOfDeck pid card | pid == player.id -> pure $ player & deckL %~ (card :)
    PutFromHandOnBottomOfDeck pid cid | pid == player.id -> do
      let (moved, kept) = partition ((== cid) . (.id)) player.hand
      pure $ player & handL .~ kept & deckL <>~ moved
    ShuffleFromDiscardIntoDeck pid cid | pid == player.id -> do
      let (moved, kept) = partition ((== cid) . (.id)) player.discard
      deck <- shuffleM (moved <> player.deck)
      pure $ player & discardL .~ kept & deckL .~ deck
    ShuffleCardIntoDeck pid card | pid == player.id -> do
      deck <- shuffleM (card : player.deck)
      pure $ player & deckL .~ deck
    ShufflePlayerDeck pid | pid == player.id -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    ShuffleHandIntoDeck pid | pid == player.id -> do
      deck <- shuffleM (player.hand <> player.deck)
      pure $ player & handL .~ [] & deckL .~ deck
    ShuffleTopOfDiscardIntoDeck pid n | pid == player.id -> do
      moved <- movableDiscardCards (take n player.discard)
      deck <- shuffleM (moved <> player.deck)
      pure $ player & discardL %~ filter (`notElem` moved) & deckL .~ deck
    DiscardChosenFromHand source pid selected | pid == player.id -> do
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      let discarded = [c | not barred, c <- discardableHand player, c.id `elem` selected]
      ownDiscard <- discardToOwners player discarded
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromHand pid c.id | c <- discarded]
      push $ DiscardedChosenFromHand source pid discarded
      pure $ player & handL %~ filter (`notElem` discarded) & discardL %~ (ownDiscard <>)
    request | Just (pid, n, resultSource, follow) <- randomDiscardRequest request, pid == player.id -> do
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      discarded <- if barred then pure [] else take n <$> shuffleM (discardableHand player)
      ownDiscard <- discardToOwners player discarded
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromHand pid c.id | c <- discarded]
      for_ resultSource \source -> push $ DiscardedRandomFromHand source pid discarded
      for_ follow \next -> push $ WithDiscardedRandomCards pid discarded next
      pure $ player & handL %~ filter (`notElem` discarded) & discardL %~ (ownDiscard <>)
    -- An ally may be in another player's hand, as after Groping Horror.
    PutAllyIntoPlayUnder _ card | any ((== card.id) . (.id)) player.hand -> pure $ player & handL %~ filter ((/= card.id) . (.id))
    -- a set-aside card with the Encounter keyword, discarded from the encounter deck, is
    -- set aside no more
    PutInDiscard card | card.owner == Just player.id -> pure $ player & discardL %~ (PlayerCard card :) & setAsideCardsL %~ filter ((/= card.id) . (.id))
    AddCollectionCardToHand pid code | pid == player.id -> do
      choices <- collectionEventChoices pid
      if code `elem` map toCardCode choices
        then do
          card <- genPlayerCard (toCardDef code) (Just pid)
          pure $ player & handL <>~ [PlayerCard card] & deckbuildingCardsL %~ ((code, 1) :)
        else pure player
    PutInHand card | card.owner == Just player.id -> pure $ player & handL <>~ [PlayerCard card]
    -- A boon event may remove itself after resolving, when it is already in discard.
    RemoveFromGame (CardIdTarget cid) ->
      pure $ player
        & handL %~ filter ((/= cid) . (.id))
        & deckL %~ filter ((/= cid) . (.id))
        & discardL %~ filter ((/= cid) . (.id))
        & setAsideCardsL %~ filter ((/= cid) . (.id))
    RemoveFromHand pid cid | pid == player.id -> pure $ player & handL %~ filter ((/= cid) . (.id))
    DiscardEncounterCardFromHand pid cid | pid == player.id -> do
      let discarded = [ec | EncounterCard ec <- player.hand, ec.id == cid]
      pushWhen (notNull discarded) $ AddToEncounterDiscard discarded
      pure $ player & handL %~ filter ((/= cid) . (.id))
    DiscardFromHand pid cid | pid == player.id -> do
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      let discarded = if barred then [] else filter ((== cid) . (.id)) (discardableHand player)
      ownDiscard <- discardToOwners player discarded
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromHand pid cid]
      pure $ player & handL %~ filter (`notElem` discarded) & discardL %~ (ownDiscard <>)
    LowerThreat pid n | pid == player.id -> pure $ player & threatL %~ max 0 . subtract n
    UseAbility pid ab _
      | pid == player.id
      , Just limitType <- abilityLimitType ab.abilityLimit -> do
          source <- limitSource ab
          let same u = u.usedAbilitySource == source && u.usedAbilityIndex == ab.abilityIndex
              used = case find same player.usedAbilities of
                Just u -> u {usedTimes = u.usedTimes + 1} : filter (not . same) player.usedAbilities
                Nothing -> UsedAbility source ab.abilityIndex limitType 1 : player.usedAbilities
          pure $ player & usedAbilitiesL .~ used
    EndPhase _ -> pure $ player & usedAbilitiesL %~ filter ((/= PerPhase) . (.usedAbilityLimitType))
    -- an enemy's attack has resolved
    CheckWindows ws | notNull [() | Window After (EnemyAttacking _ _) <- ws] ->
      pure $ player & usedAbilitiesL %~ filter ((/= PerAttack) . (.usedAbilityLimitType))
    EndRound -> pure $ player & usedAbilitiesL %~ filter ((/= PerRound) . (.usedAbilityLimitType))
    -- the heroes are picked one by one, then pay together, as one payment
    SpendResourcesFromDistinctHeroes pid sphere n used | pid == player.id -> do
      payers <- filter ((`notElem` used) . (.id)) <$> payingHeroes sphere player
      push
        $ if n <= 0 || null payers
          then SpendResourcesFrom pid [(HeroTarget h, 1) | h <- reverse used]
          else
            questionLabel
              (ikeyWith "label.payFromDifferentHero" [intVar "count" n])
              pid
              [targetLabel x [SpendResourcesFromDistinctHeroes pid sphere (n - 1) (x.id : used)] | x <- payers]
      pure player
    SpendResourcesFromOneHero pid sphere n | pid == player.id && n > 0 -> do
      payers <- filter ((>= n) . (.resources)) <$> payingHeroes sphere player
      push
        $ chooseOrRunOne pid [targetLabel x [SpendResourcesFrom pid [(toTarget x, n)]] | x <- payers]
      pure player
    SpendResourcesFrom pid pools | pid == player.id -> do
      let paid = [(t, k) | (t, k) <- pools, k > 0]
      pushAll [RemoveTokens (toSource player) t Resource k | (t, k) <- paid]
      -- a card's cost opens the window once it is paid, as 'PaidCost'
      payingForCard <- any ((== pid) . (.activeCostPlayer)) <$> getsGame gameActiveCosts
      unless payingForCard do
        spent <- for paid \(t, k) -> (,k) . fromMaybe pid <$> characterController t
        pushWhen (notNull spent)
          $ CheckWindows [mkAfter $ ResourcesSpent p (sum ks) | (p, ks) <- Map.toList (Map.fromListWith (<>) [(p, [k]) | (p, k) <- spent])]
      pure player
    SpendResources pid sphere n | pid == player.id && n > 0 -> do
      heroes <- payerHeroes player.id =<< payingFor player.id
      payers <- filterM (paysFor sphere) heroes
      push $ case filter ((> 0) . (.resources)) payers of
        [x] -> SpendResourcesFrom pid [(toTarget x, n)]
        _ -> SpendResourcesChosen pid False sphere n []
      pure player
    SpendResourcesChosen pid False sphere n chosen | pid == player.id -> do
      heroes <- payerHeroes player.id =<< payingFor player.id
      payers <- filterM (paysFor sphere) heroes
      push $ spendChosen pid False sphere n chosen [(h.id, h.resources) | h <- payers]
      pure player
    _ -> pure player
   where
    -- asks even with nothing to play or use, so the player moves the game on, not the engine
    -- with nothing to do, it says so, and names the phase that answering moves on to
    actionWindow prompt done next again = do
      playable <- playableCards player
      actions <- getActionAbilities player.id
      fromHand <- handActions player
      let idle = null playable && null actions && null fromHand
          done' = case next of
            Just phase | idle -> ikey ("label.proceedTo." <> phaseKey phase)
            _ -> done
      push
        $ playerWindow (if idle then ikey "label.noRemainingActions" else prompt) again player.id
        $ Label done' []
        : [targetLabel c [PlayCard player.id c.id, again] | c <- playable]
          <> [AbilityLabel player.id ab [] [again] | ab <- actions]
          <> [targetLabel c [UseHandAction player.id c.id, again] | c <- fromHand]
    takeFromHand cid = maybe (error "not able to find card") pure $ find ((== cid) . (.id)) player.hand

-- | Cards in the player's hand whose action from there can be used now, as Bofur's.
handActions :: HasGame m => Player -> m [Card]
handActions p = do
  allowed <- actionsAndResponsesAllowed
  if allowed then handActionsAllowed p else pure []

handActionsAllowed :: HasGame m => Player -> m [Card]
handActionsAllowed p = flip filterM p.hand \case
  c@(PlayerCard pc) | c.cardType == AllyType -> case (toAttrs $ createAlly pc (AllyId UUID.nil) p.id).allyHandAction of
    Just criteria -> andM [passesCriteriaFor p.id criteria, passesUnique c]
    Nothing -> pure False
  _ -> pure False

-- | The cards the player could play now: from hand, and from the discard pile those that
-- may be played from there.
playableCards :: HasGame m => Player -> m [Card]
playableCards p = do
  viaRecord <- traverse (discardPlayers p.id) p.discard
  -- no card leaves a discard pile The Power of Angmar holds, played or not
  locked <- elem DiscardPilesLocked <$> getModifiers GameTarget
  fromTop <- topOfDeckPlayers p
  filterM (canPlay p)
    $ filter (from [FromHand, FromHandOrDiscard]) p.hand
    <> [c | not locked, (c, rs) <- zip p.discard viaRecord, from [FromHandOrDiscard, FromDiscardOnly] c || notNull rs]
    -- the top card of the deck, as if it was in hand, as Gandalf lets it be
    <> [c | not (null fromTop), c <- take 1 p.deck, from [FromHand, FromHandOrDiscard] c]
 where
  from zones c = cdPlayableFrom (toCardDef c) `elem` zones

-- | The sources of the 'MayPlayTopOfDeck' the player has: he may play the top card of his
-- deck as if it was in his hand.
topOfDeckPlayers :: HasGame m => Player -> m [Source]
topOfDeckPlayers p = do
  mods <- getsGame (Map.findWithDefault [] (PlayerTarget p.id) . gameModifiers)
  pure [source | Modifier source MayPlayTopOfDeck <- mods]

-- | The cards whose 'MayPlayFromDiscard' lets the player play the card from their discard
-- pile, as a Record's Action does.
discardPlayers :: HasGame m => PlayerId -> Card -> m [Target]
discardPlayers pid c = do
  mods <- getsGame (Map.findWithDefault [] (PlayerTarget pid) . gameModifiers)
  pure [t | Modifier source (MayPlayFromDiscard m) <- mods, cardMatches m c, Just t <- [sourceTarget source]]

canPlay :: HasGame m => Player -> Card -> m Bool
canPlay p c = andM [canAfford p c, passesUnique c, correctPhase p.id c, hasPlayableHost p.id c, canPayAdditional p c, not <$> cannotPlay p.id c, not <$> barredFromHand p c]

-- | Whether the card, in the player's hand, gives itself 'CannotBePlayedFromHand'.
barredFromHand :: HasGame m => Player -> Card -> m Bool
barredFromHand p c = do
  fromTop <- notNull <$> topOfDeckPlayers p
  let fromHand = c.id `elem` map (.id) p.hand || (fromTop && Just c.id == fmap (.id) (headMay p.deck))
  capped <- getsGame (flip handEntryLimitReached p.id)
  prohibited <- elem CannotBePlayedFromHand <$> getModifiers (CardIdTarget c.id)
  pure $ fromHand && (capped || prohibited)

-- | Whether the player has what the card costs from hand besides its resources, and may
-- pay it: one who cannot discard from hand, as Isen River bars, cannot pay a discard.
canPayAdditional :: HasGame m => Player -> Card -> m Bool
canPayAdditional p c = case cdAdditionalCost (toCardDef c) of
  Just (DiscardCardsFromHand n) | c.id `elem` map (.id) p.hand -> do
    barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget p.id)
    pure $ not barred && length [x | x <- discardableHand p, x.id /= c.id] >= n
  _ -> pure True

-- | Whether the game forbids playing the card; a Master card too, but to the player who
-- controls The One Ring (A Shadow in the East rules, "The One Ring & The Master Trait").
cannotPlay :: HasGame m => PlayerId -> Card -> m Bool
cannotPlay pid c =
  orM
    [ andM [pure $ c.cardType == EventType, not <$> actionsAndResponsesAllowed]
    , notNull <$> playProhibitions pid c
    , andM [pure $ Master `member` cdCardTraits (toCardDef c), not <$> controlsTheOneRing pid]
    ]

canAfford :: HasGame m => Player -> Card -> m Bool
canAfford p c = do
  -- an attachment costs what the host that lowers it most leaves
  discount <- foldr max 0 <$> (traverse (attachDiscount c) =<< attachableHosts p.id c)
  canAffordWith discount p c

-- | Whether the player can afford the card with its cost lowered by so much, as the host
-- it is to go on lowers it.
canAffordWith :: HasGame m => Int -> Player -> Card -> m Bool
canAffordWith discount p c = case (c.cost, c.sphere) of
  (Just cost, Just printed) -> do
    -- one that does not require a resource match is paid for as a neutral card
    free <- any (\case NextIgnoresResourceMatch m -> cardMatches m c; _ -> False) <$> getModifiers (PlayerTarget p.id)
    let sphere = if free then Neutral else printed
    extra <- subtract discount <$> costModifier p.id c
    heroes <- payerHeroes p.id [c]
    payers <-
      filterM (\h -> andM [heroCanSpend h, not <$> barredFrom c h, orM [paysFor sphere h, iconFor c sphere h, noMatchNeeded c]]) [h | h <- heroes, h.resources > 0]
    fromAllies <- sum . map snd <$> allyPayersFor p.id c
    -- a card that costs 0 still needs a resource match (FAQ 2.39, 2.61): a hero with its
    -- resource icon, which paying with resources of another sphere (A Good Harvest) isn't;
    -- a printed icon the hero has while paying for the card is, as Bard son of Brand's
    -- and so is a card that needs no resource match (Radagast's Creature allies)
    matched <- orM [pure (sphere == Neutral), noMatchNeeded c, anyM (\h -> orM [matchesSphere sphere h, printedIconFor c sphere h]) heroes]
    let needed = costAsResources cost + extra
    case cdDistinctPayers (toCardDef c) of
      Just n -> pure $ length payers >= n + extra
      Nothing | cdSinglePayer (toCardDef c) -> pure $ any ((>= needed) . (.resources)) payers
      Nothing
        | needed <= 0 -> pure $ matched || fromAllies > 0
        | otherwise -> pure $ sum (map (.resources) payers) + fromAllies >= needed
  _ -> pure False
 where
  matchesSphere = heroHasResourceIcon

{- | What modifiers add to the cost of the card the player would play: the game's, the
player's own and the card's (negative when they lower it), less its secrecy while the
player's threat is 20 or lower.
-}
costModifier :: HasGame m => PlayerId -> Card -> m Int
costModifier pid c = do
  mods <-
    concat
      <$> sequence
        [getModifiers GameTarget, getModifiers (PlayerTarget pid), getModifiers (CardIdTarget c.id)]
  threat <- getThreat pid
  let secrecy = if threat <= 20 then sum [n | Secrecy n <- toList (cdKeywords $ toCardDef c)] else 0
  -- the characters it may exhaust to cost less count too, as The Houses of Healing's Healers
  exhaustable <- for [t | ReducedByExhausting t <- mods] \t ->
    length <$> exhaustableCharacters (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait t) (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait t)
  pure $ subtract (secrecy + sum exhaustable) $ sum $ flip mapMaybe mods \case
    IncreaseCost m n | cardMatches m c -> Just n
    ReduceCostOf m n | cardMatches m c -> Just (negate n)
    ReduceCostOfToMinimumOne m n | cardMatches m c -> Just (negate $ min n $ maybe 0 (max 0 . subtract 1 . costAsResources) c.cost)
    CostModifier n -> Just n
    -- an optional reduction counts toward whether the card can be afforded
    ReducedByDiscardingFromDeck n -> Just (negate n)
    ReduceNextCostOf m n | cardMatches m c -> Just (negate $ min n $ maybe 0 (max 0 . subtract 1 . costAsResources) c.cost)
    LowerNextCostOf m n | cardMatches m c -> Just (negate n)
    _ -> Nothing

-- | Heroes with a resource that could pay for a card of the sphere.
payingHeroes :: HasGame m => Sphere -> Player -> m [Hero]
payingHeroes sphere player = do
  heroes <- payerHeroes player.id =<< payingFor player.id
  filterM (paysFor sphere) [h | h <- heroes, h.resources > 0]

-- | The cards the player is paying for.
payingFor :: HasGame m => PlayerId -> m [Card]
payingFor pid = getsGame (map (.activeCostCard) . filter ((== pid) . (.activeCostPlayer)) . toList . gameActiveCosts)

-- | The heroes whose resources can pay for the cards: those the player controls, and those
-- a card's 'PayableFromHeroes' names, as Burglar Baggins may be paid for from Bilbo
-- Baggins' pool by a player who does not control him.
payerHeroes :: HasGame m => PlayerId -> [Card] -> m [Hero]
payerHeroes pid cards = do
  mods <- concatMapM (getModifiers . CardIdTarget . (.id)) cards
  own <- select (HeroControlledBy $ PlayerWithId pid)
  others <- concatMapM select [m | PayableFromHeroes m <- mods]
  traverse getHero (nub $ own <> others)

hasPlayableHost :: HasGame m => PlayerId -> Card -> m Bool
hasPlayableHost pid c
  | PlayerCard pc <- c
  , isAttachmentType c.cardType
  , attachMatcherFor pid (toCardCode c) `notElem` [PlayIntoStagingArea, PlayIntoPlayArea] =
      andM [passesCriteria (attachmentCriteriaFor pid pc), notNull <$> playableCardHosts pid (toCardCode c)]
  | otherwise = hasHost pid c

hasHost :: HasGame m => PlayerId -> Card -> m Bool
hasHost pid c
  | PlayerCard pc <- c
  , isAttachmentType c.cardType =
      if attachMatcherFor pid (toCardCode c) `elem` [PlayIntoStagingArea, PlayIntoPlayArea]
        then pure True
        -- an attachment's play criteria are checked here too, with its hosts
        else andM [passesCriteria (attachmentCriteriaFor pid pc), notNull <$> attachableHosts pid c]
  | otherwise = pure True

-- | Where an attachment could go when played by this player.
attachableHosts :: HasGame m => PlayerId -> Card -> m [Target]
attachableHosts pid = \case
  c@(PlayerCard _) | isAttachmentType c.cardType -> cardHosts pid (toCardCode c)
  _ -> pure []

cardOwner :: PlayerCard -> PlayerId
cardOwner = fromMaybe (PlayerId UUID.nil) . (.owner)

correctPhase :: HasGame m => PlayerId -> Card -> m Bool
correctPhase pid (PlayerCard card) = case cdCardType (toCardDef card) of
  AllyType -> isPhase PlanningPhase
  AttachmentType -> isPhase PlanningPhase
  TreasureType -> isPhase PlanningPhase
  PlayerSideQuestType -> isPhase PlanningPhase
  -- an action event can be played in any action window; responses are offered by theirs
  EventType -> actionEventPlayable pid card
  _ -> pure False
correctPhase _ (EncounterCard _) = pure False

-- | Whether an event is an action whose criteria let it be played now.
actionEventPlayable :: HasGame m => PlayerId -> PlayerCard -> m Bool
actionEventPlayable pid card = do
  allowed <- actionsAndResponsesAllowed
  if allowed then actionEventPlayableAllowed pid card else pure False

actionEventPlayableAllowed :: HasGame m => PlayerId -> PlayerCard -> m Bool
actionEventPlayableAllowed pid card = case lookupEvent (toCardCode card) card.id pid of
  Just e | Nothing <- (toAttrs e).eventWindow -> passesCriteria (toAttrs e).eventCriteria
  Just e | Just criteria <- (toAttrs e).eventActionCriteria -> passesCriteria criteria
  _ -> pure False

{- | Whether a card could be played with nothing to pay for it, at any time: as the
unique rule, an attachment's hosts and an event's criteria allow.
-}
canPlayAtNoCost :: HasGame m => PlayerId -> Card -> m Bool
canPlayAtNoCost pid c = andM [passesUnique c, hasHost pid c, eventOk]
 where
  eventOk = case c of
    PlayerCard pc | c.cardType == EventType -> actionEventPlayable pid pc
    _ -> pure True

isPhase :: HasGame m => Phase -> m Bool
isPhase phase = (== phase) <$> getsGame gamePhase

resourcesInSphere :: HasGame m => Sphere -> Player -> m Int
resourcesInSphere sphere player = do
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
  sum . map (.resources) <$> filterM (paysFor sphere) heroes

{- | Whether a hero's resources can pay for a card of the sphere: its own sphere, or one
a resource icon it has gained gives it.
-}
paysFor :: HasGame m => Sphere -> Hero -> m Bool
paysFor sphere h = do
  cards <- getsGame (map (.activeCostCard) . toList . gameActiveCosts)
  andM [not <$> anyM (`barredFrom` h) cards, orM [heroPaysFor sphere h, andM [heroCanSpend h, orM ([iconFor c sphere h | c <- cards] <> map noMatchNeeded cards)]]]

-- | Whether the hero's resources cannot pay for the card, as Na'asiyah's for allies.
barredFrom :: HasGame m => Card -> Hero -> m Bool
barredFrom c h = any (\case CannotPayFor m -> cardMatches m c; _ -> False) <$> getModifiers h

-- | Whether the hero has the sphere's resource icon while paying for the card.
iconFor :: HasGame m => Card -> Sphere -> Hero -> m Bool
iconFor c sphere h = do
  lost <- elem (LoseResourceIcon sphere) <$> getModifiers h
  if lost then pure False else orM
    [ any (\case GainResourceIconFor m s -> s == sphere && cardMatches m c; _ -> False) <$> getModifiers h
    , printedIconFor c sphere h
    ]

-- | Whether the hero is considered to have the sphere's printed resource icon while paying
-- for the card.
printedIconFor :: HasGame m => Card -> Sphere -> Hero -> m Bool
printedIconFor c sphere h = do
  own <- getModifiers h
  pure $ LoseResourceIcon sphere `notElem` own && any (\case PrintedResourceIconFor m s -> s == sphere && cardMatches m c; PrintedResourceIconForCard cid s -> s == sphere && cid == c.id; _ -> False) own

-- | Whether the card's cost may be paid with resources of any sphere.
noMatchNeeded :: HasGame m => Card -> m Bool
noMatchNeeded c = elem NoResourceMatchNeeded <$> getModifiers (CardIdTarget c.id)

startingHandSize :: Int
startingHandSize = 6

-- | The starting hand, with what changes it, as Gandalf's Delay has each player draw 1 less.
getStartingHandSize :: HasGame m => m Int
getStartingHandSize = (\ms -> max 0 $ startingHandSize + sum [n | StartingHandSize n <- ms]) <$> getModifiers GameTarget

-- | Playing through a card action bypasses normal ally/attachment phase timing,
-- while preserving payment, unique, host, additional-cost and event criteria.
playableCardsByEffect :: HasGame m => PlayerId -> CardMatcher -> Int -> m [Card]
playableCardsByEffect pid matcher reduction = do
  player <- getPlayer pid
  filterM (\c -> andM [canAffordWith reduction player c, canPlayAtNoCost pid c, canPayAdditional player c, not <$> cannotPlay pid c, not <$> barredFromHand player c])
    [c | c <- player.hand, cardMatches matcher c]

-- | Cards an effect can play from the deck, with the stated discount, outside
-- ordinary ally/attachment timing. Restrictions specific to hand do not apply.
playableCardsFromDeckByEffect :: HasGame m => PlayerId -> Int -> m [Card]
playableCardsFromDeckByEffect pid reduction = do
  player <- getPlayer pid
  filterM (\c -> andM [canAffordWith reduction player c, canPlayAtNoCost pid c, canPayAdditional player c, not <$> cannotPlay pid c])
    [c | c@PlayerCard {} <- player.deck]

-- The physical deck's construction list persists through ordinary movement of
-- cards. Collection additions extend it; retrieving an existing discard does not.
collectionEventChoices :: HasGame m => PlayerId -> m [CardDef]
collectionEventChoices pid = do
  player <- getPlayer pid
  let ident = CardId UUID.nil
  let original = if null player.playerDeckbuildingCards
        then [(toCardCode c, 1) | c <- player.deck <> player.hand <> player.discard]
        else player.playerDeckbuildingCards
      eligible def =
        cdCardType def == EventType && isJust (lookupEvent (toCardCode def) ident pid)
          && isNothing (deckProblem $ DeckList player.startingHeroes ((toCardCode def, 1) : original) player.errata player.playerMessengerOfTheKingAlly (toCardCode <$> player.contract))
  pure $ filter eligible (toList allPlayerCards)

-- Global changes (such as Gandalf's Delay) and this player's ship both apply.
getStartingHandSizeFor :: HasGame m => PlayerId -> m Int
getStartingHandSizeFor pid = do
  mods <- (<>) <$> getModifiers GameTarget <*> getModifiers (PlayerTarget pid)
  pure $ max 0 $ startingHandSize + sum [n | StartingHandSize n <- mods]
