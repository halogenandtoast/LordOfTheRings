module LordOfTheRings.Player.Runner (module LordOfTheRings.Player.Runner, module X) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.ActiveCost (ActiveCost (..))
import LordOfTheRings.Effect
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Event
import LordOfTheRings.Frontend (FrontendMessage (..), send)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (getActionAbilities, passesCriteria)
import LordOfTheRings.Modifier
import LordOfTheRings.Hero.Types
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Entities (entitiesTreacheries)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.Types ()
import LordOfTheRings.Log.Block (LogBlockKind (..))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Player.Types as X
import LordOfTheRings.I18n
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage Player where
  runMessage msg player = case msg of
    DoStep 1 Setup -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    DoStep 2 Setup -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
      pure $ player & threatL .~ sum (map (.threat) heroes)
    DoStep 5 Setup -> do
      push
        $ questionLabel (ikey "label.keepStartingHand") player.id
          [ Label (ikey "label.keepHand") []
          , Label (ikey "label.mulligan") [Mulligan player.id]
          ]
      let (hand, deck) = splitAt startingHandSize player.deck
      pure $ player & handL .~ hand & deckL .~ deck
    DiscardEventsFromHand pid | pid == player.id -> do
      let (events, rest) = partition ((== EventType) . (.cardType)) player.hand
      pure $ player & handL .~ rest & discardL %~ (events <>)
    Mulligan pid | pid == player.id -> do
      send $ ShuffleIntoDeck $ map (.id) player.hand
      (hand, deck) <- splitAt startingHandSize <$> shuffleM (player.hand <> player.deck)
      pure $ player & handL .~ hand & deckL .~ deck
    RaiseThreat pid n | pid == player.id -> do
      push CheckEliminations
      pure $ player & threatL %~ max 0 . (+ n)
    EliminatePlayer pid | pid == player.id -> do
      pure $ player & eliminatedL .~ True & discardL %~ (player.hand <>) & handL .~ []
    BeginPhase ResourcePhase -> do
      push $ DrawCards $ Drawing player.id 1
      pure player
    DrawCards drawing | drawing.player == player.id -> do
      blocked <- elem CannotDrawCards <$> getModifiers player
      -- a condition on top of the deck is discarded in place of the next draw
      shackles <- getsGame (filter ((== AttachedTo (toTarget player)) . (.placement)) . toList . entitiesTreacheries . gameEntities)
      pushAll [Discard (toTarget t) | t <- take 1 shackles]
      let (cards, deck) = splitAt drawing.count player.deck
      pure $ if blocked || notNull shackles then player else player & handL <>~ cards & deckL .~ deck
    WithTarget (isTarget player -> True) (DoPhase PlanningPhase) -> do
      actionWindow (ikey "label.planningActions") (ikey "label.donePlanning") $ WithTarget (toTarget player) (DoPhase PlanningPhase)
      pure player
    ActionWindow pid | pid == player.id -> do
      actionWindow (ikey "label.takeAction") (ikey "label.pass") $ ActionWindow pid
      pure player
    PlayCard pid cid | pid == player.id -> do
      push $ PlayCardInWindow pid cid []
      pure player
    PlayCardInWindow pid cid windows | pid == player.id -> do
      card <- takeFromHand cid
      pushAll =<< inLogBlock (PlayBlock pid card) [PayForCard pid card windows]
      pure $ player & handL %~ filter ((/= cid) . (.id))
    PutAllyIntoPlay pid cid | pid == player.id -> do
      takeFromHand cid >>= \case
        PlayerCard card -> push $ PlayAlly pid card
        EncounterCard _ -> error "not an ally"
      pure $ player & handL %~ filter ((/= cid) . (.id))
    PlayEvent pid card _ | pid == player.id -> pure $ player & discardL %~ (PlayerCard card :)
    RemoveFromDiscard pid cid | pid == player.id -> pure $ player & discardL %~ filter ((/= cid) . (.id))
    ShuffleDiscardIntoDeck pid | pid == player.id -> do
      deck <- shuffleM (player.deck <> player.discard)
      pure $ player & deckL .~ deck & discardL .~ []
    TakeFromDeck pid cid | pid == player.id -> do
      let (taken, kept) = partition ((== cid) . (.id)) player.deck
      pure $ player & deckL .~ kept & handL <>~ taken
    DebugSearchDeck pid | pid == player.id -> do
      -- ponytail: no shuffle after, debug searches leave the deck order alone
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
        $ [targetLabel (CardIdTarget c.id) (TakeFromDeck pid c.id : again c <> [ShufflePlayerDeck pid | not anyNumber]) | c <- inDeck, cardMatches matcher c]
        <> [Label (ikey "label.done") [ShufflePlayerDeck pid]]
      pure player
    ChooseFromSearchIntoPlay pid source cards matcher n mods | pid == player.id && n > 0 -> do
      let inDeck = filter ((`elem` map (.id) player.deck) . (.id)) cards
      allies <- filterM passesUnique [c | c <- inDeck, c.cardType == AllyType, cardMatches matcher c]
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.searchPutIntoPlay") (sourceTarget source) []
        $ SearchCards inDeck
        $ ChooseOne
        $ [ targetLabel (CardIdTarget c.id)
              $ [CreateEffect $ Effect source (CardIdTarget c.id) mods UntilEndOfRound | notNull mods]
              <> [TakeFromDeck pid c.id, PutAllyIntoPlay pid c.id, ChooseFromSearchIntoPlay pid source (filter ((/= c.id) . (.id)) inDeck) matcher (n - 1) mods]
          | c <- allies
          ]
        <> [Label (ikey "label.done") [ShufflePlayerDeck pid]]
      pure player
    ChooseFromSearchIntoPlay pid _ _ _ _ _ | pid == player.id -> do
      push $ ShufflePlayerDeck pid
      pure player
    DiscardTopOfDeck pid n | pid == player.id -> do
      let (discarded, deck) = splitAt n player.deck
      pure $ player & deckL .~ deck & discardL %~ (discarded <>)
    SwapHandWithDeck pid fromHand fromDeck | pid == player.id -> do
      let swap c
            | c.id == fromHand = fromMaybe c $ find ((== fromDeck) . (.id)) player.deck
            | c.id == fromDeck = fromMaybe c $ find ((== fromHand) . (.id)) player.hand
            | otherwise = c
      pure $ player & handL %~ map swap & deckL %~ map swap
    ShufflePlayerDeck pid | pid == player.id -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    DiscardRandomFromHand pid n | pid == player.id -> do
      (discarded, kept) <- splitAt n <$> shuffleM player.hand
      pure $ player & handL .~ kept & discardL %~ (discarded <>)
    PutInDiscard card | card.owner == Just player.id -> pure $ player & discardL %~ (PlayerCard card :)
    DiscardFromHand pid cid | pid == player.id -> do
      let (discarded, kept) = partition ((== cid) . (.id)) player.hand
      pure $ player & handL .~ kept & discardL %~ (discarded <>)
    LowerThreat pid n | pid == player.id -> pure $ player & threatL %~ max 0 . subtract n
    UseAbility pid ab _ | pid == player.id, Just limitType <- abilityLimitType ab.abilityLimit -> do
      let same u = u.usedAbilitySource == ab.abilitySource && u.usedAbilityIndex == ab.abilityIndex
          used = case find same player.usedAbilities of
            Just u -> u {usedTimes = u.usedTimes + 1} : filter (not . same) player.usedAbilities
            Nothing -> UsedAbility ab.abilitySource ab.abilityIndex limitType 1 : player.usedAbilities
      pure $ player & usedAbilitiesL .~ used
    EndPhase _ -> pure $ player & usedAbilitiesL %~ filter ((/= PerPhase) . (.usedAbilityLimitType))
    EndRound -> pure $ player & usedAbilitiesL %~ filter ((/= PerRound) . (.usedAbilityLimitType))
    SpendResourcesFromDistinctHeroes pid sphere n used | pid == player.id && n > 0 -> do
      payers <- filter ((`notElem` used) . (.id)) <$> payingHeroes sphere player
      push
        $ questionLabel (ikeyWith "label.payFromDifferentHero" [intVar "count" n]) pid
          [ targetLabel x [RemoveTokens (toSource player) (toTarget x) Resource 1, SpendResourcesFromDistinctHeroes pid sphere (n - 1) (x.id : used)]
          | x <- payers
          ]
      pure player
    SpendResources pid sphere n | pid == player.id && n > 0 -> do
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
      payers <- filterM (paysFor sphere) heroes
      case filter ((> 0) . (.resources)) payers of
        [x] -> push $ RemoveTokens (toSource player) (toTarget x) Resource n
        xs ->
          push
            $ questionLabel (ikeyWith "label.payMore" [intVar "count" n]) pid
              [ targetLabel x [RemoveTokens (toSource player) (toTarget x) Resource 1, SpendResources pid sphere (n - 1)]
              | x <- xs
              ]
      pure player
    _ -> pure player
   where
    -- asks even with nothing to play or use, so the player moves the game on, not the engine
    actionWindow prompt done again = do
      playable <- filterM (canPlay player) player.hand
      actions <- getActionAbilities player.id
      push
        $ playerWindow prompt again player.id
        $ Label done []
        : [targetLabel c [PlayCard player.id c.id, again] | c <- playable]
        <> [AbilityLabel player.id ab [] [again] | ab <- actions]
    takeFromHand cid = maybe (error "not able to find card") pure $ find ((== cid) . (.id)) player.hand

canPlay :: (HasGame m) => Player -> Card -> m Bool
canPlay p c = andM [canAfford p c, passesUnique c, correctPhase c, hasHost c, not <$> cannotPlay p.id c]

-- | Whether the game forbids playing the card.
cannotPlay :: (HasGame m) => PlayerId -> Card -> m Bool
cannotPlay pid c = any (\case CannotPlay m -> cardMatches m c; _ -> False) <$> ((<>) <$> getModifiers GameTarget <*> getModifiers (PlayerTarget pid))

canAfford :: (HasGame m) => Player -> Card -> m Bool
canAfford p c = case (c.cost, c.sphere) of
  (Just cost, Just sphere) -> do
    extra <- costModifier p.id c
    heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p.id)
    payers <- filterM (\h -> orM [paysFor sphere h, iconFor c sphere h]) [h | h <- heroes, h.resources > 0]
    case cdDistinctPayers (toCardDef c) of
      Just n -> pure $ length payers >= n + extra
      Nothing -> do
        fromAllies <- sum . map snd <$> allyPayersFor p.id c
        pure $ sum (map (.resources) payers) + fromAllies >= costAsResources cost + extra
  _ -> pure False

-- | What modifiers add to the cost of the card the player would play: the game's, the
-- player's own and the card's (negative when they lower it).
costModifier :: (HasGame m) => PlayerId -> Card -> m Int
costModifier pid c = do
  mods <- concat <$> sequence [getModifiers GameTarget, getModifiers (PlayerTarget pid), getModifiers (CardIdTarget c.id)]
  pure $ sum $ flip mapMaybe mods \case
    IncreaseCost m n | cardMatches m c -> Just n
    ReduceCostOf m n | cardMatches m c -> Just (negate n)
    CostModifier n -> Just n
    _ -> Nothing

-- | Heroes with a resource that could pay for a card of the sphere.
payingHeroes :: (HasGame m) => Sphere -> Player -> m [Hero]
payingHeroes sphere player = do
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
  filterM (paysFor sphere) [h | h <- heroes, h.resources > 0]

hasHost :: (HasGame m) => Card -> m Bool
hasHost c
  | c.cardType == AttachmentType = notNull <$> attachmentHosts (attachMatcherFor $ toCardCode c)
  | otherwise = pure True

correctPhase :: (HasGame m) => Card -> m Bool
correctPhase (PlayerCard card) = case cdCardType (toCardDef card) of
  AllyType -> andM [isPhase PlanningPhase, allyAllowed]
  AttachmentType -> isPhase PlanningPhase
  -- an action event can be played in any action window; responses are offered by theirs
  EventType -> case card.owner >>= lookupEvent (toCardCode card) card.id of
    Just e | Nothing <- (toAttrs e).eventWindow -> passesCriteria (toAttrs e).eventCriteria
    _ -> pure False
  _ -> pure False
correctPhase (EncounterCard _) = pure False

isPhase :: (HasGame m) => Phase -> m Bool
isPhase phase = (== phase) <$> getsGame gamePhase

resourcesInSphere :: (HasGame m) => Sphere -> Player -> m Int
resourcesInSphere sphere player = do
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId player.id)
  sum . map (.resources) <$> filterM (paysFor sphere) heroes

-- | Whether a hero's resources can pay for a card of the sphere: its own sphere, or one
-- a resource icon it has gained gives it.
paysFor :: (HasGame m) => Sphere -> Hero -> m Bool
paysFor sphere h = orM [heroPaysFor sphere h, paying]
 where
  paying = do
    cards <- getsGame (map (.activeCostCard) . toList . gameActiveCosts)
    orM [iconFor c sphere h | c <- cards]

-- | Whether the hero has the sphere's resource icon while paying for the card.
iconFor :: (HasGame m) => Card -> Sphere -> Hero -> m Bool
iconFor c sphere h = any (\case GainResourceIconFor m s -> s == sphere && cardMatches m c; _ -> False) <$> getModifiers h

startingHandSize :: Int
startingHandSize = 6

-- | Whether an ally may be played under a one-ally-per-round rule.
allyAllowed :: (HasGame m) => m Bool
allyAllowed = do
  mods <- getModifiers GameTarget
  pure $ not (OneAllyPerRound `elem` mods && AllyPlayedThisRound `elem` mods)
