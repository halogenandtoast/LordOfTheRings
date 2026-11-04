{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Player.Runner (module LordOfTheRings.Player.Runner, module X) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.ActiveCost (ActiveCost (..), playCard)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import LordOfTheRings.Ally (createAlly)
import LordOfTheRings.Ally.Types (AllyAttrs (allyHandAction))
import LordOfTheRings.Campaign (campaignLogThreatPenalty)
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
import LordOfTheRings.Helpers.Modifiers (getModifiers, modifiedValue, playProhibitions, threatOf)
import LordOfTheRings.Helpers.Window (exhaustableCharacters, getActionAbilities, limitSource, passesCriteria, passesCriteriaFor, travelableLocations)
import LordOfTheRings.Hero.Types
import LordOfTheRings.I18n
import LordOfTheRings.Keyword (Keyword (Doomed, Secrecy))
import LordOfTheRings.Id (AllyId (..), PlayerId)
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
import LordOfTheRings.Treachery.Types ()
import LordOfTheRings.Window (
  Window (Window),
  WindowType (CardsDrawn, DiscardedFromDeck, DrewForResourcePhase, SetupHandDrawn, DiscardedFromHand, EnemyAttacking, PlayedCard, PlayedCardFromHand, PlayedFromHand, ThreatRaised),
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
      penalty <- getsGame (maybe 0 campaignLogThreatPenalty . scenarioCampaign . toAttrs . gameScenario)
      pure $ player & threatL .~ threat + penalty & startingThreatL .~ threat + penalty
    DoStep 5 Setup -> do
      -- a response instead of a mulligan, as Galdor of the Havens', takes the question away
      push $ CheckWindows [mkAfter $ SetupHandDrawn player.id]
      push
        $ questionLabel
          (ikey "label.keepStartingHand")
          player.id
          [ Label (ikey "label.keepHand") []
          , Label (ikey "label.mulligan") [Mulligan player.id]
          ]
      -- a card a hero's Setup put in hand stays there, as Thurindir's side quest (Caleb's
      -- ruling: it is searched for before the setup hand, and kept through a mulligan)
      let (hand, deck) = splitAt startingHandSize player.deck
      pure $ player & handL <>~ hand & deckL .~ deck
    DiscardEventsFromHand pid | pid == player.id -> do
      let (events, rest) = partition ((== EventType) . (.cardType)) player.hand
      pure $ player & handL .~ rest & discardL %~ (events <>)
    Mulligan pid | pid == player.id -> do
      -- only the setup hand, the last cards in hand, goes back
      let (kept, drawn) = splitAt (length player.hand - startingHandSize) player.hand
      send $ ShuffleIntoDeck $ map (.id) drawn
      (hand, deck) <- splitAt startingHandSize <$> shuffleM (drawn <> player.deck)
      pure $ player & handL .~ kept <> hand & deckL .~ deck
    -- a raise opens a window before it, as Harbor Thug's Forced effect answers
    RaiseThreat pid n | pid == player.id, n > 0 -> do
      pushAll [CheckWindows [mkWhen $ ThreatRaised pid], DoRaiseThreat pid n]
      pure player
    RaiseThreat pid n | pid == player.id -> do
      push CheckEliminations
      pure $ player & threatL %~ max 0 . (+ n)
    DoRaiseThreat pid n | pid == player.id -> do
      pushAll [CheckEliminations, CheckWindows [mkAfter $ ThreatRaised pid]]
      pure $ player & threatL %~ max 0 . (+ n)
    EliminatePlayer pid | pid == player.id -> do
      level <- getEliminationLevel pid
      pure $ player & threatL .~ level & eliminatedL .~ True & discardL %~ (player.hand <>) & handL .~ []
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
      pushAll [Discard (toTarget t) | t <- take 1 shackles]
      unless (blocked || notNull shackles) $ push $ DoDrawCards drawing
      pure player
    DoDrawCards drawing | drawing.player == player.id -> do
      let (cards, deck) = splitAt drawing.count player.deck
          -- a hero in the deck was shuffled in by Lost and Alone, and is put into play as drawn
          (heroes, drawn) = partition ((== HeroType) . (.cardType)) cards
      pushAll [PutHeroIntoPlay player.id pc | PlayerCard pc <- heroes]
      pushWhen (notNull cards) $ CheckWindows [mkAfter $ CardsDrawn player.id (length cards)]
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
      card <- maybe (error "not able to find card") pure $ find ((== cid) . (.id)) (player.hand <> player.discard)
      let fromHand = cid `elem` map (.id) player.hand
          ownDiscard = cdPlayableFrom (toCardDef card) `elem` [FromHandOrDiscard, FromDiscardOnly]
      -- a card in the discard pile a Record lets be played: the Record is discarded for it,
      -- and the card goes to the bottom of the deck after
      records <- if fromHand || ownDiscard then pure [] else discardPlayers pid card
      -- what it costs from hand besides its resources is paid first, as Guardian of
      -- Rivendell's discards
      let additional = [ChooseAndDiscardFromHand pid (CardIdSource cid) n | fromHand, Just (DiscardCardsFromHand n) <- [cdAdditionalCost (toCardDef card)]]
      played <- inLogBlock (PlayBlock pid card) (additional <> [PayForCard pid card fromHand windows])
      push $ case records of
        [] -> Run played
        _ -> chooseOrRunOne pid [targetLabel r (Discard r : played <> [PutDiscardOnBottomOfDeck pid cid]) | r <- records]
      pure $ player & handL %~ filter ((/= cid) . (.id)) & discardL %~ filter ((/= cid) . (.id))
    PutAllyIntoPlay pid cid | pid == player.id -> do
      takeFromHand cid >>= \case
        PlayerCard card -> do
          -- put into play, a card with Doomed raises each player's threat as one played does
          raiseThreatFromDoomed $ sum [n | Doomed n <- toList (cdKeywords $ toCardDef card)]
          push $ PlayAlly pid card
        EncounterCard _ -> error "not an ally"
      pure $ player & handL %~ filter ((/= cid) . (.id))
    -- attached without being played, as Plundered Armoury lets one be
    PutAttachmentIntoPlayFromHand pid cid host | pid == player.id -> do
      takeFromHand cid >>= \case
        PlayerCard card -> push $ PlayAttachmentOn pid card host
        EncounterCard _ -> error "not an attachment"
      pure $ player & handL %~ filter ((/= cid) . (.id))
    PlayEvent pid card _ | pid == player.id -> pure $ player & discardL %~ (PlayerCard card :)
    -- an event whose cost is X, played for no cost, has X of 0 and so does nothing, as
    -- Men of the West played with Vilya (ruling)
    PlayAtNoCost pid card
      | pid == player.id
      , cdCardType (toCardDef card) == EventType
      , cdCost (toCardDef card) == Just DynamicCost -> do
          push $ CheckWindows [mkAfter $ PlayedCard pid card]
          pure $ player & discardL %~ (PlayerCard card :)
    PlayAtNoCost pid card | pid == player.id -> do
      pushAll $ playCard pid card [] <> [CheckWindows [mkAfter $ PlayedCard pid card]]
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
    PlayTopOfDeckAtNoCostOrBottom pid source | pid == player.id -> do
      for_ (headMay player.deck) \case
        top@(PlayerCard pc) -> do
          able <- canPlayAtNoCost top
          -- an ally or attachment is put into play, not played: only an event is played
          -- (ruling, Vilya)
          let putIntoPlay = top.cardType `elem` [AllyType, AttachmentType]
              resolve = case top.cardType of
                AllyType -> [PutAllyIntoPlayUnder pid pc]
                AttachmentType -> [PutAttachmentIntoPlay pid pc]
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
    -- a card placed underneath another leaves wherever its owner had it
    PlaceUnderneath _ cards -> do
      let gone c = c.id `elem` map (.id) cards
      pure $ player & handL %~ filter (not . gone) & deckL %~ filter (not . gone) & discardL %~ filter (not . gone)
    ShuffleDiscardIntoDeck pid | pid == player.id -> do
      deck <- shuffleM (player.deck <> player.discard)
      pure $ player & deckL .~ deck & discardL .~ []
    TakeFromDeck pid cid | pid == player.id -> do
      let (taken, kept) = partition ((== cid) . (.id)) player.deck
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
    DiscardTopOfDeck pid n | pid == player.id -> do
      let (discarded, deck) = splitAt n player.deck
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromDeck pid c.id | c <- discarded]
      pure $ player & deckL .~ deck & discardL %~ (discarded <>)
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
    ShufflePlayerDeck pid | pid == player.id -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    ShuffleTopOfDiscardIntoDeck pid n | pid == player.id -> do
      let (moved, kept) = splitAt n player.discard
      deck <- shuffleM (moved <> player.deck)
      pure $ player & discardL .~ kept & deckL .~ deck
    DiscardRandomFromHand pid n | pid == player.id -> do
      discarded <- take n <$> shuffleM (discardableHand player)
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromHand pid c.id | c <- discarded]
      pure $ player & handL %~ filter (`notElem` discarded) & discardL %~ (discarded <>)
    -- an ally put into play from its owner's hand under any player's control, as Emery
    PutAllyIntoPlayUnder _ card | card.owner == Just player.id -> pure $ player & handL %~ filter ((/= card.id) . (.id))
    -- a set-aside card with the Encounter keyword, discarded from the encounter deck, is
    -- set aside no more
    PutInDiscard card | card.owner == Just player.id -> pure $ player & discardL %~ (PlayerCard card :) & setAsideCardsL %~ filter ((/= card.id) . (.id))
    PutInHand card | card.owner == Just player.id -> pure $ player & handL <>~ [PlayerCard card]
    RemoveFromHand pid cid | pid == player.id -> pure $ player & handL %~ filter ((/= cid) . (.id))
    DiscardFromHand pid cid | pid == player.id -> do
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      let discarded = if barred then [] else filter ((== cid) . (.id)) (discardableHand player)
      pushWhen (notNull discarded) $ CheckWindows [mkAfter $ DiscardedFromHand pid cid]
      pure $ player & handL %~ filter (`notElem` discarded) & discardL %~ (discarded <>)
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
    SpendResourcesFromDistinctHeroes pid sphere n used | pid == player.id && n > 0 -> do
      payers <- filter ((`notElem` used) . (.id)) <$> payingHeroes sphere player
      push
        $ questionLabel
          (ikeyWith "label.payFromDifferentHero" [intVar "count" n])
          pid
          [ targetLabel
              x
              [ RemoveTokens (toSource player) (toTarget x) Resource 1
              , SpendResourcesFromDistinctHeroes pid sphere (n - 1) (x.id : used)
              ]
          | x <- payers
          ]
      pure player
    SpendResourcesFromOneHero pid sphere n | pid == player.id && n > 0 -> do
      payers <- filter ((>= n) . (.resources)) <$> payingHeroes sphere player
      push
        $ chooseOrRunOne pid [targetLabel x [RemoveTokens (toSource player) (toTarget x) Resource n] | x <- payers]
      pure player
    SpendResources pid sphere n | pid == player.id && n > 0 -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
      payers <- filterM (paysFor sphere) heroes
      push $ case filter ((> 0) . (.resources)) payers of
        [x] -> RemoveTokens (toSource player) (toTarget x) Resource n
        _ -> SpendResourcesChosen pid False sphere n []
      pure player
    SpendResourcesChosen pid False sphere n chosen | pid == player.id -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
      payers <- filterM (paysFor sphere) heroes
      push $ spendChosen (toSource player) pid False sphere n chosen [(h.id, h.resources) | h <- payers]
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
handActions p = flip filterM p.hand \case
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
  filterM (canPlay p)
    $ filter (from [FromHand, FromHandOrDiscard]) p.hand
    <> [c | not locked, (c, rs) <- zip p.discard viaRecord, from [FromHandOrDiscard, FromDiscardOnly] c || notNull rs]
 where
  from zones c = cdPlayableFrom (toCardDef c) `elem` zones

-- | The cards whose 'MayPlayFromDiscard' lets the player play the card from their discard
-- pile, as a Record's Action does.
discardPlayers :: HasGame m => PlayerId -> Card -> m [Target]
discardPlayers pid c = do
  mods <- getsGame (Map.findWithDefault [] (PlayerTarget pid) . gameModifiers)
  pure [t | Modifier source (MayPlayFromDiscard m) <- mods, cardMatches m c, Just t <- [sourceTarget source]]

canPlay :: HasGame m => Player -> Card -> m Bool
canPlay p c = andM [canAfford p c, passesUnique c, correctPhase c, hasHost c, canPayAdditional p c, not <$> cannotPlay p.id c]

-- | Whether the player has what the card costs from hand besides its resources, and may
-- pay it: one who cannot discard from hand, as Isen River bars, cannot pay a discard.
canPayAdditional :: HasGame m => Player -> Card -> m Bool
canPayAdditional p c = case cdAdditionalCost (toCardDef c) of
  Just (DiscardCardsFromHand n) | c.id `elem` map (.id) p.hand -> do
    barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget p.id)
    pure $ not barred && length [x | x <- discardableHand p, x.id /= c.id] >= n
  _ -> pure True

-- | Whether the game forbids playing the card.
cannotPlay :: HasGame m => PlayerId -> Card -> m Bool
cannotPlay pid c = notNull <$> playProhibitions pid c

canAfford :: HasGame m => Player -> Card -> m Bool
canAfford p c = case (c.cost, c.sphere) of
  (Just cost, Just sphere) -> do
    -- an attachment costs what the host that lowers it most leaves
    discount <- foldr max 0 <$> (traverse (attachDiscount c) =<< attachableHosts c)
    extra <- subtract discount <$> costModifier p.id c
    heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p.id)
    payers <-
      filterM (\h -> andM [heroCanSpend h, not <$> barredFrom c h, orM [paysFor sphere h, iconFor c sphere h]]) [h | h <- heroes, h.resources > 0]
    fromAllies <- sum . map snd <$> allyPayersFor p.id c
    -- a card that costs 0 still needs a resource match (FAQ 2.39, 2.61): a hero with its
    -- resource icon, which paying with resources of another sphere (A Good Harvest) isn't
    matched <- (sphere == Neutral ||) <$> anyM (matchesSphere sphere) heroes
    let needed = costAsResources cost + extra
    case cdDistinctPayers (toCardDef c) of
      Just n -> pure $ length payers >= n + extra
      Nothing | cdSinglePayer (toCardDef c) -> pure $ any ((>= needed) . (.resources)) payers
      Nothing
        | needed <= 0 -> pure $ matched || fromAllies > 0
        | otherwise -> pure $ sum (map (.resources) payers) + fromAllies >= needed
  _ -> pure False
 where
  matchesSphere sphere h = (\own -> h.sphere == sphere || GainResourceIcon sphere `elem` own) <$> getModifiers h

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
    CostModifier n -> Just n
    -- an optional reduction counts toward whether the card can be afforded
    ReducedByDiscardingFromDeck n -> Just (negate n)
    ReduceNextCostOf m n | cardMatches m c -> Just (negate $ min n $ maybe 0 (max 0 . subtract 1 . costAsResources) c.cost)
    LowerNextCostOf m n | cardMatches m c -> Just (negate n)
    _ -> Nothing

-- | Heroes with a resource that could pay for a card of the sphere.
payingHeroes :: HasGame m => Sphere -> Player -> m [Hero]
payingHeroes sphere player = do
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
  filterM (paysFor sphere) [h | h <- heroes, h.resources > 0]

hasHost :: HasGame m => Card -> m Bool
hasHost c
  | c.cardType == AttachmentType, attachMatcherFor (toCardCode c) `elem` [PlayIntoStagingArea, PlayIntoPlayArea] = pure True
  -- an attachment's play criteria are checked here too, with its hosts
  | PlayerCard pc <- c
  , c.cardType == AttachmentType =
      andM [passesCriteria (attachmentCriteriaFor pc), notNull <$> attachableHosts c]
  | otherwise = pure True

-- | Where an attachment card could go, none for any other card.
attachableHosts :: HasGame m => Card -> m [Target]
attachableHosts c
  | c.cardType == AttachmentType = cardHosts (toCardCode c)
  | otherwise = pure []

correctPhase :: HasGame m => Card -> m Bool
correctPhase (PlayerCard card) = case cdCardType (toCardDef card) of
  AllyType -> isPhase PlanningPhase
  AttachmentType -> isPhase PlanningPhase
  PlayerSideQuestType -> isPhase PlanningPhase
  -- an action event can be played in any action window; responses are offered by theirs
  EventType -> actionEventPlayable card
  _ -> pure False
correctPhase (EncounterCard _) = pure False

-- | Whether an event is an action whose criteria let it be played now.
actionEventPlayable :: HasGame m => PlayerCard -> m Bool
actionEventPlayable card = case card.owner >>= lookupEvent (toCardCode card) card.id of
  Just e | Nothing <- (toAttrs e).eventWindow -> passesCriteria (toAttrs e).eventCriteria
  Just e | Just criteria <- (toAttrs e).eventActionCriteria -> passesCriteria criteria
  _ -> pure False

{- | Whether a card could be played with nothing to pay for it, at any time: as the
unique rule, an attachment's hosts and an event's criteria allow.
-}
canPlayAtNoCost :: HasGame m => Card -> m Bool
canPlayAtNoCost c = andM [passesUnique c, hasHost c, eventOk]
 where
  eventOk = case c of
    PlayerCard pc | c.cardType == EventType -> actionEventPlayable pc
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
  andM [not <$> anyM (`barredFrom` h) cards, orM [heroPaysFor sphere h, andM [heroCanSpend h, orM [iconFor c sphere h | c <- cards]]]]

-- | Whether the hero's resources cannot pay for the card, as Na'asiyah's for allies.
barredFrom :: HasGame m => Card -> Hero -> m Bool
barredFrom c h = any (\case CannotPayFor m -> cardMatches m c; _ -> False) <$> getModifiers h

-- | Whether the hero has the sphere's resource icon while paying for the card.
iconFor :: HasGame m => Card -> Sphere -> Hero -> m Bool
iconFor c sphere h =
  any (\case GainResourceIconFor m s -> s == sphere && cardMatches m c; _ -> False) <$> getModifiers h

startingHandSize :: Int
startingHandSize = 6
