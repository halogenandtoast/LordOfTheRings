module LordOfTheRings.Game.Public (publicGame, publicGameFor) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base
import LordOfTheRings.Card
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Modifiers (playProhibitions)
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Id
import LordOfTheRings.Log.Entry (LogRef)
import LordOfTheRings.Log.Refs (sourceRefFor)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (Modifier (..))
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Quest.Types (QuestAttrs (..))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Target
import LordOfTheRings.Prelude

-- | The game as the players may see it: decks are hidden information, so only their
-- sizes are sent. Stored games and exports keep everything. The quest tally is added
-- so the board shows the engine's totals, modifiers included, and which cards add to
-- the threat (and each card's stats and hit points as modified, and the attack being
-- declared), as are the hosts each attachment in hand could go on, so dragging one only
-- lights up those.
publicGame :: Game -> Value
publicGame = publicGameFor (const True)

-- | The game as someone sees it who may only look at the hands of the players given:
-- the rest send only their sizes, and their questions do not show the cards searched.
--
-- Separated, every area's players and cards are sent as the game's, and "areas" says
-- which cards are in each area, in turn order, with its staging threat: the quest tally
-- is the area in focus's.
publicGameFor :: (PlayerId -> Bool) -> Game -> Value
publicGameFor seesHand g = hideDecks $ toJSON merged
 where
  merged = g {gamePlayers = allPlayers g, gameEntities = g.entities <> mconcat (heldEntities g)}
  -- the game as each area sees it, the area in focus first
  areaViews = g : [focusArea (areaLead g s a) g | Just s <- [gameSeparation g], a <- separationAreas s, not (isFocusedArea g a)]
  areas =
    [ object
        [ "players" .= areaPlayers a
        , "focused" .= isFocusedArea g a
        , "threat" .= runReadGame getStagingThreat v
        , "cards" .= encounterCards v.entities
        ]
    | Just s <- [gameSeparation g]
    , a <- areasInTurnOrder g s
    , v <- take 1 [v | v <- areaViews, isFocusedArea v a]
    ]
  encounterCards es =
    map ((.id) . toCard) (toList es.enemies)
      <> map ((.id) . toCard) (toList es.locations)
      <> map ((.id) . toCard) (toList es.quests)
      <> map ((.id) . toCard) (toList es.objectives)
      <> map ((.id) . toCard) (toList es.attachments)
      <> map ((.id) . toCard) (toList es.treacheries)
  hideDecks = \case
    Object o ->
      Object
        $ KeyMap.delete "separation"
        $ KeyMap.insert "areas" (toJSON areas)
        $ KeyMap.insert "committedWillpower" (toJSON $ runReadGame getCommittedWillpower g)
        $ KeyMap.insert "stagingThreat" (toJSON $ runReadGame getStagingThreat g)
        $ KeyMap.insert "threatening" (toJSON [cid | v <- areaViews, (cid, n) <- runReadGame getStagingThreats v, n > 0])
        $ KeyMap.insert "stats" (toJSON $ foldMap (runReadGame cardStats) areaViews)
        $ KeyMap.insert "hitPoints" (toJSON $ foldMap (runReadGame hitPoints) areaViews)
        $ KeyMap.insert "attackStrength" (toJSON $ runReadGame attackStrength g)
        $ KeyMap.insert "attachmentHosts" (toJSON attachHosts)
        $ KeyMap.insert "playProhibitions" (toJSON prohibitions)
        $ KeyMap.insert "underneath" (toJSON underneath)
        $ KeyMap.insert "captured" (toJSON $ fmap length (fromMeta "captured" :: Map CardId [Value]))
        $ KeyMap.insert "capturedDecks" (toJSON $ fmap length (fromMeta "capturedDecks" :: Map PlayerId [Value]))
        $ adjust (inside "objectives" $ overValues $ inside "underneath" $ const $ Array mempty) "entities"
        $ adjust (overEntries \pid -> if seesHand pid then id else hideSearch) "question"
        $ adjust (overEntries \pid -> if seesHand pid then hidePlayer else hideHand . hidePlayer) "players"
        $ adjust hideScenario "scenario" o
    v -> v
  -- the stats printed down a card's side, as they are now
  cardStats :: ReadGame (Map CardId [(Text, Int)])
  cardStats = do
    heroes <- select AnyHero >>= traverse \i -> character (toTarget i) =<< getHero i
    allies <- select AnyAlly >>= traverse \i -> character (toTarget i) =<< getAlly i
    enemies <- select AnyEnemy >>= traverse \i -> do
      e <- getEnemy i
      stats <- sequence [("threat",) <$> getEnemyThreat i, ("attack",) <$> getEnemyAttack i, ("defense",) <$> getEnemyDefense i]
      pure ((toCard e).id, stats)
    locations <- select AnyLocation >>= traverse \i -> do
      l <- getLocation i
      threat <- getLocationThreat i
      pure ((toCard l).id, [("threat", threat)])
    pure $ mapFromList $ heroes <> allies <> enemies <> locations
  hitPoints :: ReadGame (Map CardId Int)
  hitPoints = do
    heroes <- select AnyHero >>= traverse \i -> (,) . (.id) . toCard <$> getHero i <*> getHitPoints (toTarget i)
    allies <- select AnyAlly >>= traverse \i -> (,) . (.id) . toCard <$> getAlly i <*> getHitPoints (toTarget i)
    enemies <- select AnyEnemy >>= traverse \i -> (,) . (.id) . toCard <$> getEnemy i <*> getEnemyHitPoints i
    pure $ mapFromList $ heroes <> allies <> enemies
  -- while characters are declared as attackers, the attack they would make on each enemy
  attackStrength :: ReadGame (Map CardId Int)
  attackStrength = do
    attackers <- (<>) <$> (map toTarget <$> select AttackingHero) <*> (map toTarget <$> select AttackingAlly)
    if null attackers
      then pure mempty
      else fmap mapFromList $ select AnyEnemy >>= traverse \i -> do
        e <- getEnemy i
        ((toCard e).id,) <$> getAttackStrength attackers i (toCardDef e)
  character target c = do
    stats <- sequence [("willpower",) <$> getWillpower target, ("attack",) <$> getAttack target, ("defense",) <$> getDefense target]
    pure ((toCard c).id, stats)
  attachHosts :: Map CardId [Target]
  attachHosts =
    mapFromList
      [ (c.id, runReadGame (cardHosts $ toCardCode c) v)
      | v <- areaViews
      , p <- toList v.players
      , seesHand p.id
      , c <- p.hand
      , c.cardType == AttachmentType
      ]
  -- the cards in hand an effect forbids playing, with what is forbidding them: empty
  -- when it cannot be named, the card is still forbidden
  prohibitions :: Map CardId [LogRef]
  prohibitions =
    mapFromList
      [ (c.id, runReadGame (nub . catMaybes <$> traverse (sourceRefFor . modifierSource) mods) v)
      | v <- areaViews
      , p <- toList v.players
      , seesHand p.id
      , c <- p.hand
      , let mods = runReadGame (playProhibitions p.id c) v
      , notNull mods
      ]
  -- the cards facedown under each card, counted, by its card id: those under an
  -- objective, as The Orcs' Search, and those a scenario keeps under its cards in its
  -- meta's "underneath", as The Steward's Fear's underworld cards under its locations
  underneath :: Map CardId Int
  underneath =
    Map.filter (> 0)
      $ Map.unionWith
        (+)
        ( mapFromList
            $ [(o.objectiveCardId, length o.objectiveUnderneath) | o <- map toAttrs (toList $ entitiesObjectives merged.entities)]
            <> [(q.questCardId, length q.questUnderneath) | q <- map toAttrs (toList $ entitiesQuests merged.entities)]
        )
        (maybe mempty (fmap length) (parseMaybe (withObject "meta" (.: "underneath")) (scenarioMeta $ toAttrs g.scenario) :: Maybe (Map CardId [Value])))
  -- what the scenario keeps by this key in its meta, as Escape from Mount Gram's captured
  -- cards, under each card and in each player's captured deck
  fromMeta :: FromJSON a => Monoid a => Key.Key -> a
  fromMeta k = fromMaybe mempty $ parseMaybe (withObject "meta" (.: k)) (scenarioMeta $ toAttrs g.scenario)
  hidePlayer = withSize "deck" "deckSize"
  hideHand = \case
    Object o | Just (Array cards) <- KeyMap.lookup "hand" o ->
      Object $ KeyMap.insert "handSize" (toJSON $ length cards) $ KeyMap.insert "hand" (Array mempty) o
    v -> v
  -- a search's cards hang off its question, which may be inside labels
  hideSearch = \case
    Object o -> Object $ adjust hideSearch "question" $ if KeyMap.member "cards" o then KeyMap.insert "cards" (Array mempty) o else o
    v -> v
  -- a scenario's own state holds what is facedown, as the underworld deck
  hideScenario = hideMeta . hideHidden . withSize "questDeck" "questDeckSize" . withSize "encounterDeck" "encounterDeckSize"
  hideMeta = \case
    Object o -> Object $ KeyMap.delete "meta" o
    v -> v
  -- hidden cards are facedown: only their ids are sent, so the board can lay them out
  hideHidden = \case
    Object o -> Object $ adjust (overEntries \(_ :: PlayerId) -> idsOnly) "hiddenCards" o
    v -> v
  idsOnly = \case
    Array cards -> Array $ fmap (\case Object c -> fromMaybe Null (KeyMap.lookup "id" c); v -> v) cards
    v -> v
  withSize key sizeKey = \case
    Object o | Just (Array cards) <- KeyMap.lookup key o ->
      Object $ KeyMap.insert sizeKey (toJSON $ length cards) $ KeyMap.delete key o
    v -> v
  adjust f k o = maybe o (\v -> KeyMap.insert k (f v) o) (KeyMap.lookup k o)
  inside k f = \case
    Object o -> Object $ adjust f k o
    v -> v
  overValues f = \case
    Object o -> Object $ fmap f o
    v -> v
  -- keyed by player
  overEntries f = \case
    Object o -> Object $ KeyMap.mapWithKey (\k -> maybe id f (parseMaybe parseJSON (String $ Key.toText k))) o
    v -> v
