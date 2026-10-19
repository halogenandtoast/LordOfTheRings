module LordOfTheRings.Player.Runner (module LordOfTheRings.Player.Runner, module X) where

import LordOfTheRings.Ability
import LordOfTheRings.Card
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Window
import LordOfTheRings.Hero.Types
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
    RunWindow window -> do
      abilities <- getWindowAbilities window
      when (notNull abilities) $ do
        let (forced, nonForced) = partition isForcedAbility abilities
        if null forced
          then push $ chooseOne player.id [AbilityLabel player.id ab [] | ab <- nonForced]
          else push $ Ask player.id $ ChooseOneAtATime [AbilityLabel player.id ab [] | ab <- forced]
      pure player
    DoStep 1 Setup -> do
      deck <- shuffleM player.deck
      pure $ player & deckL .~ deck
    DoStep 2 Setup -> do
      heroes <- select $ PlayerHero player.id
      pure $ player & threatL .~ sum (map (.threat) heroes)
    DoStep 5 Setup -> do
      let (hand, deck) = splitAt 6 player.deck
      pure $ player & handL .~ hand & deckL .~ deck
    BeginPhase ResourcePhase -> do
      push $ DrawCards $ Drawing player.id 1
      pure player
    DrawCards drawing | drawing.player == player.id -> do
      let (cards, deck) = splitAt drawing.count player.deck
      pure $ player & handL <>~ cards & deckL .~ deck
    WithTarget (isTarget player -> True) (DoPhase PlanningPhase) -> do
      playable <- filterM (canPlay player) player.hand
      unless (null playable) $
        push
          $ chooseOne player.id
          $ Label "Done Planning" []
          : [ targetLabel c [PlayCard player.id c.id, WithTarget (toTarget player) (DoPhase PlanningPhase)]
            | c <- playable
            ]
      pure player
    PlayCard pid cid | pid == player.id -> do
      case find ((== cid) . (.id)) player.hand of
        Nothing -> error "not able to find card"
        Just card -> do
          push $ PayForCard pid card
          pure $ player & handL %~ filter ((/= cid) . (.id))
    SpendResources pid sphere n | pid == player.id && n > 0 -> do
      heroes <- select $ PlayerHero player.id
      let sphereMatches h = sphere == Neutral || h.sphere == sphere
      case filter (\h -> sphereMatches h && h.resources > 0) heroes of
        [x] -> push $ RemoveTokens (toSource player) (toTarget x) Resource n
        xs ->
          push
            $ chooseOne pid
              [ targetLabel x [RemoveTokens (toSource player) (toTarget x) Resource 1, SpendResources pid sphere (n - 1)]
              | x <- xs
              ]
      pure player
    _ -> pure player

canPlay :: (HasGame m) => Player -> Card -> m Bool
canPlay p c = case c.cost of
  Nothing -> pure False
  Just cost ->
    andM
      [ maybe (pure False) (\s -> (>= costAsResources cost) <$> resourcesInSphere s p) c.sphere
      , passesUnique c
      , correctPhase c
      ]

passesUnique :: (HasGame m) => Card -> m Bool
passesUnique card
  | card.unique = andM [selectNone $ AllyWithName card.name, selectNone $ HeroWithName card.name]
  | otherwise = pure True

correctPhase :: (HasGame m) => Card -> m Bool
correctPhase (PlayerCard card) = case cdCardType (toCardDef card) of
  AllyType -> isPhase PlanningPhase
  AttachmentType -> isPhase PlanningPhase
  _ -> pure True
correctPhase (EncounterCard _) = pure False

isPhase :: (HasGame m) => Phase -> m Bool
isPhase phase = (== phase) <$> getsGame gamePhase

resourcesInSphere :: (HasGame m) => Sphere -> Player -> m Int
resourcesInSphere sphere player = do
  heroes <- select $ PlayerHero player.id
  pure $ sum . map (.resources) $ filter (\h -> sphere == Neutral || h.sphere == sphere) heroes
