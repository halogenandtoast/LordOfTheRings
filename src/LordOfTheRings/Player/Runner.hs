module LordOfTheRings.Player.Runner (module LordOfTheRings.Player.Runner, module X) where

import LordOfTheRings.Ability.Types
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
import LordOfTheRings.Entities (entitiesTreacheries)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.Types ()
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Player.Types as X
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
        $ questionLabel "Keep this starting hand?" player.id
          [ Label "Keep this hand" []
          , Label "Mulligan: shuffle all six back and draw again" [Mulligan player.id]
          ]
      let (hand, deck) = splitAt startingHandSize player.deck
      pure $ player & handL .~ hand & deckL .~ deck
    DiscardEventsFromHand pid | pid == player.id -> do
      let (events, rest) = partition ((== EventType) . (.cardType)) player.hand
      pure $ player & handL .~ rest & discardL <>~ events
    Mulligan pid | pid == player.id -> do
      send $ ShuffleIntoDeck $ map (.id) player.hand
      (hand, deck) <- splitAt startingHandSize <$> shuffleM (player.hand <> player.deck)
      pure $ player & handL .~ hand & deckL .~ deck
    RaiseThreat pid n | pid == player.id -> do
      push CheckEliminations
      pure $ player & threatL %~ max 0 . (+ n)
    EliminatePlayer pid | pid == player.id -> do
      pure $ player & eliminatedL .~ True & discardL <>~ player.hand & handL .~ []
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
      actionWindow "Play cards or use actions" "Done Planning" $ WithTarget (toTarget player) (DoPhase PlanningPhase)
      pure player
    ActionWindow pid | pid == player.id -> do
      actionWindow "Take an action?" "Pass" $ ActionWindow pid
      pure player
    PlayCard pid cid | pid == player.id -> do
      push $ PlayCardInWindow pid cid []
      pure player
    PlayCardInWindow pid cid windows | pid == player.id -> do
      card <- takeFromHand cid
      push $ PayForCard pid card windows
      pure $ player & handL %~ filter ((/= cid) . (.id))
    PutAllyIntoPlay pid cid | pid == player.id -> do
      takeFromHand cid >>= \case
        PlayerCard card -> push $ PlayAlly pid card
        EncounterCard _ -> error "not an ally"
      pure $ player & handL %~ filter ((/= cid) . (.id))
    PlayEvent pid card _ | pid == player.id -> pure $ player & discardL <>~ [PlayerCard card]
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
        $ QuestionLabel "Take a card into your hand" Nothing []
        $ SearchCards player.deck
        $ ChooseOne
        $ Label "Done" []
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
    ShufflePlayerDeck pid | pid == player.id -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    DiscardRandomFromHand pid n | pid == player.id -> do
      (discarded, kept) <- splitAt n <$> shuffleM player.hand
      pure $ player & handL .~ kept & discardL <>~ discarded
    DiscardFromHand pid cid | pid == player.id -> do
      let (discarded, kept) = partition ((== cid) . (.id)) player.hand
      pure $ player & handL .~ kept & discardL <>~ discarded
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
        $ questionLabel ("Pay 1 from a different hero (" <> tshow n <> " more)") pid
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
            $ questionLabel ("Pay " <> tshow n <> " more") pid
              [ targetLabel x [RemoveTokens (toSource player) (toTarget x) Resource 1, SpendResources pid sphere (n - 1)]
              | x <- xs
              ]
      pure player
    _ -> pure player
   where
    -- only asks when there is something to play or use
    actionWindow prompt done again = do
      playable <- filterM (canPlay player) player.hand
      actions <- getActionAbilities player.id
      unless (null playable && null actions)
        $ push
        $ questionLabel prompt player.id
        $ Label done []
        : [targetLabel c [PlayCard player.id c.id, again] | c <- playable]
        <> [AbilityLabel player.id ab [] [again] | ab <- actions]
    takeFromHand cid = maybe (error "not able to find card") pure $ find ((== cid) . (.id)) player.hand

canPlay :: (HasGame m) => Player -> Card -> m Bool
canPlay p c = andM [canAfford p c, passesUnique c, correctPhase c, hasHost c]

canAfford :: (HasGame m) => Player -> Card -> m Bool
canAfford p c = case (c.cost, c.sphere) of
  (Just cost, Just sphere) -> case cdDistinctPayers (toCardDef c) of
    Just n -> (>= n) . length <$> payingHeroes sphere p
    Nothing -> (>= costAsResources cost) <$> resourcesInSphere sphere p
  _ -> pure False

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
paysFor sphere h
  | sphere == Neutral || h.sphere == sphere = pure True
  | otherwise = elem (GainResourceIcon sphere) <$> getModifiers h

startingHandSize :: Int
startingHandSize = 6

-- | Whether an ally may be played under a one-ally-per-round rule.
allyAllowed :: (HasGame m) => m Bool
allyAllowed = do
  mods <- getModifiers GameTarget
  pure $ not (OneAllyPerRound `elem` mods && AllyPlayedThisRound `elem` mods)
