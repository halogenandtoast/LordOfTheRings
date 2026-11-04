{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Game.Public (GroupScore (..), groupScore, hiddenCards, publicGame, publicGameFor, revealsInformation) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import LordOfTheRings.Campaign (campaignCardFor)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesContracts, entitiesLocations, entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base
import LordOfTheRings.Game.SearchZones (searchZones)
import LordOfTheRings.Card
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Modifiers (playProhibitions)
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Id
import LordOfTheRings.Log.Entry (LogRef)
import LordOfTheRings.Enemy.Types (EnemyAttrs (..))
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId, locationUnderneath), isUncharted)
import LordOfTheRings.Log.Refs (sourceRefFor)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (Modifier (..))
import LordOfTheRings.Contract.Types (ContractAttrs (..))
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Quest.Types (QuestAttrs (..))
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Question (Question (QuestionLabel), UI (TargetLabel), questionChoices, searchedCards)
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

-- | The group's score (Revised Core rulebook, "Scoring"); lower is better.
data GroupScore = GroupScore {threat :: Int, deadHeroes :: Int, damage :: Int, rounds :: Int, victoryPoints :: Int, total :: Int}
  deriving stock Generic
  deriving anyclass ToJSON

groupScore :: Game -> GroupScore
groupScore g = GroupScore finalThreat deadThreat damage rounds victory (finalThreat + deadThreat + damage + rounds - victory)
 where
  merged = g {gamePlayers = allPlayers g, gameEntities = g.entities <> mconcat (heldEntities g)}
  finalThreat = sum [p.threat | p <- toList merged.players]
  -- Dead heroes remain in their owner's discard; eliminated players also lose
  -- heroes outside play (for example a hero shuffled into their deck).
  deadThreat = sum
    [ fromMaybe 0 (cdThreatCost $ toCardDef c)
    | p <- toList merged.players
    , c <- p.discard <> (if p.eliminated then p.hand <> p.deck <> p.setAsideCards else [])
    , cdCardType (toCardDef c) == HeroType
    ]
  damage = sum [h.damage | h <- toList merged.entities.heroes, maybe False (not . (.eliminated)) (Map.lookup h.owner merged.players)]
  -- 10 for each round, as the campaign log scores it (Scenario.Runner)
  rounds = 10 * g.round
  victory = (toAttrs g.scenario).victoryPoints

-- | The cards no player can see, as 'publicGameFor' hides them: decks, facedown cards
-- and shadows, Uncharted locations' other sides, less any a question is showing (a search,
-- a look at the top of a deck).
hiddenCards :: Game -> Set CardId
hiddenCards g = Set.fromList hidden `Set.difference` Set.fromList shown
 where
  merged = g {gamePlayers = allPlayers g, gameEntities = g.entities <> mconcat (heldEntities g)}
  s = toAttrs g.scenario
  hidden =
    concatMap (map (.id) . (.deck)) (toList merged.players)
      <> map (.id) (s.encounterDeck <> s.questDeck <> fromMaybe [] s.scenarioCorsairDeck <> fromMaybe [] s.evilCreaturesDeck <> s.scenarioUnchartedDeck <> s.scenarioGrottoDeck <> concat (toList s.scenarioHiddenCards))
      <> concat
        [ [c.id | c <- a.shadowCards, c.id `notElem` a.enemyShadowRevealed] <> map (.id) a.enemyFacedown
        | a <- map toAttrs (toList merged.entities.enemies)
        ]
      <> [(toCard l).id | l <- toList merged.entities.locations, isUncharted l]
      -- the loot deck facedown under The Burglar's Turn
      <> [c.id | k <- toList merged.entities.contracts, c <- (toAttrs k).contractUnderneath]
  shown = map (.id) $ concatMap searchedCards (toList g.question)

{- | The answer that took the game from one to the other brought something hidden into
view, so an undo back past it would let the players play on knowing it.
-}
revealsInformation :: Game -> Game -> Bool
revealsInformation before after = not . null $ hiddenCards before `Set.difference` hiddenCards after

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
        $ KeyMap.insert "score" (toJSON $ groupScore g)
        $ KeyMap.delete "separation"
        $ KeyMap.insert "areas" (toJSON areas)
        $ KeyMap.insert "committedWillpower" (toJSON $ runReadGame getCommittedWillpower g)
        $ KeyMap.insert "stagingThreat" (toJSON $ runReadGame getStagingThreat g)
        $ KeyMap.insert "threatening" (toJSON [cid | v <- areaViews, (cid, n) <- runReadGame getStagingThreats v, n > 0])
        $ KeyMap.insert "stats" (toJSON $ foldMap (runReadGame cardStats) areaViews)
        $ KeyMap.insert "hitPoints" (toJSON $ foldMap (runReadGame hitPoints) areaViews)
        $ KeyMap.insert "attackStrength" (toJSON $ runReadGame attackStrength g)
        $ KeyMap.insert "attackAdding" (toJSON $ runReadGame attackAdding g)
        $ KeyMap.insert "attachmentHosts" (toJSON attachHosts)
        $ KeyMap.insert "playProhibitions" (toJSON prohibitions)
        $ KeyMap.insert "underneath" (toJSON underneath)
        $ KeyMap.insert "underneathPlayer" (toJSON underneathPlayer)
        $ KeyMap.insert "searchZones" (toJSON $ searchZones g)
        $ KeyMap.insert "captured" (toJSON $ fmap length (fromMeta "captured" :: Map CardId [Value]))
        $ KeyMap.insert "capturedDecks" (toJSON $ fmap length (fromMeta "capturedDecks" :: Map PlayerId [Value]))
        -- the Stormcaller's quest stage and its progress (Flight of the Stormcaller)
        $ KeyMap.insert "stormcallerStage" (metaValue "stormcallerStage")
        $ KeyMap.insert "stormcallerProgress" (metaValue "stormcallerProgress")
        -- the heat tracker's temperature (Desert Crossing)
        $ KeyMap.insert "temperature" (metaValue "temperature")
        -- the Orcs' quest stage and its progress (Race Across Harad)
        $ KeyMap.insert "orcsStage" (metaValue "orcsStage")
        $ KeyMap.insert "orcsProgress" (metaValue "orcsProgress")
        -- the side quest deck's size, its cards kept hidden (Fire in the Night)
        $ KeyMap.insert "sideQuestDeck" (toJSON $ length (fromMeta "sideQuestDeck" :: [Value]))
        -- the damage a set-aside card keeps, as Cold-Drake's (The Withered Heath)
        $ KeyMap.insert "setAsideDamage" (toJSON (fromMeta "setAsideDamage" :: Map CardId Int))
        $ adjust (inside "objectives" $ overValues $ inside "underneath" $ const $ Array mempty) "entities"
        $ adjust (inside "contracts" $ overValues $ inside "underneath" $ const $ Array mempty) "entities"
        -- the cards under a location stay hidden, unless they lie faceup, as a Camp's Warden
        $ adjust (inside "locations" $ overValues hideFacedownUnderneath) "entities"
        $ adjust (inside "locations" $ overValues hideOtherSide) "entities"
        $ adjust (overEntries \pid -> if seesHand pid then id else hideSearch) "question"
        $ adjust (overEntries \pid -> if seesHand pid then hidePlayer else hideHand . hidePlayer) "players"
        $ adjust hideScenario "scenario" o
        -- in campaign mode, the campaign card next to the quest deck
        & KeyMap.insert "campaignCard" (toJSON campaignCard)
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
  -- while attackers are asked for, the attack each one choosable would add to the attack
  -- on the enemy, with its bonuses against it
  attackAdding :: ReadGame (Map CardId Int)
  attackAdding = fmap (mapFromList . concat) $ for (toList g.question) \case
    QuestionLabel label _ [EnemyTarget eid] q | takeWhile (/= ' ') label `elem` [ikey "label.declareAttackers", ikey "label.joinAttackRanged"] -> do
      e <- getEnemy eid
      fmap catMaybes $ for [t | TargetLabel t _ <- questionChoices q] \t -> do
        cid <- case t of
          HeroTarget i -> Just . (.id) . toCard <$> getHero i
          AllyTarget i -> Just . (.id) . toCard <$> getAlly i
          _ -> pure Nothing
        for cid \c -> (c,) <$> getAttackStrength [t] eid (toCardDef e)
    _ -> pure []
  character target c = do
    stats <- sequence [("willpower",) <$> getWillpower target, ("attack",) <$> getAttack target, ("defense",) <$> getDefense target]
    pure ((toCard c).id, stats)
  attachHosts :: Map CardId [Target]
  attachHosts =
    mapFromList
      [ (c.id, runReadGame (cardHosts p.id $ toCardCode c) v)
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
  -- the encounter cards facedown under each card, counted, by its card id: those under
  -- an objective, as The Orcs' Search, and those a scenario keeps under its cards in its
  -- meta's "underneath", as The Steward's Fear's underworld cards under its locations
  underneath :: Map CardId Int
  underneath =
    Map.filter (> 0)
      $ Map.unionWith
        (+)
        (under \c -> length [() | EncounterCard _ <- c])
        (maybe mempty (fmap length) (parseMaybe (withObject "meta" (.: "underneath")) (scenarioMeta $ toAttrs g.scenario) :: Maybe (Map CardId [Value])))
  -- and the player cards, shown with a player card back, as the hands Truly Lost places
  -- under locations
  underneathPlayer :: Map CardId Int
  underneathPlayer = Map.filter (> 0) $ under players <> mapFromList [(l.locationCardId, players l.locationUnderneath) | l <- map toAttrs (toList $ entitiesLocations merged.entities)]
  players c = length [() | PlayerCard _ <- c]
  -- what lies under objectives and quests (the encounter cards under a location are
  -- counted from the meta, where The Steward's Fear keeps them)
  under :: ([Card] -> Int) -> Map CardId Int
  under n =
    mapFromList
      $ [(o.objectiveCardId, n o.objectiveUnderneath) | o <- map toAttrs (toList $ entitiesObjectives merged.entities)]
      <> [(q.questCardId, n q.questUnderneath) | q <- map toAttrs (toList $ entitiesQuests merged.entities)]
      -- and contracts, as The Burglar's Turn's loot deck
      <> [(c.contractCardId, n c.contractUnderneath) | c <- map toAttrs (toList $ entitiesContracts merged.entities)]
  -- what the scenario keeps by this key in its meta, as Escape from Mount Gram's captured
  -- cards, under each card and in each player's captured deck
  fromMeta :: FromJSON a => Monoid a => Key.Key -> a
  fromMeta k = fromMaybe mempty $ parseMaybe (withObject "meta" (.: k)) (scenarioMeta $ toAttrs g.scenario)
  metaValue :: Key.Key -> Value
  metaValue k = fromMaybe Null $ parseMaybe (withObject "meta" (.: k)) (scenarioMeta $ toAttrs g.scenario)
  -- a location's facedown side is sent only to those who have looked at it
  hideOtherSide = \case
    Object o
      | not (any seesHand (fromMaybe [] $ parseMaybe parseJSON =<< KeyMap.lookup "lookedAt" o :: [PlayerId])) ->
          Object $ KeyMap.delete "otherSide" o
    v -> v
  hideFacedownUnderneath = \case
    v@(Object o) | KeyMap.lookup "underneathFaceup" o == Just (Bool True) -> v
    v -> inside "underneath" (const $ Array mempty) v
  -- the cards under a threat dial are facedown too, as Lost in the Dark places them
  hidePlayer = withSize "underThreatDial" "underThreatDialSize" . withSize "deck" "deckSize"
  hideHand = \case
    Object o | Just (Array cards) <- KeyMap.lookup "hand" o ->
      Object $ KeyMap.insert "handSize" (toJSON $ length cards) $ KeyMap.insert "hand" (Array mempty) o
    v -> v
  -- a search's cards hang off its question, which may be inside labels
  hideSearch = \case
    Object o -> Object $ adjust hideSearch "question" $ if KeyMap.member "cards" o then KeyMap.insert "cards" (Array mempty) o else o
    v -> v
  -- a scenario's own state holds what is facedown, as the underworld deck
  campaignCard =
    let a = toAttrs g.scenario
     in (\code -> object ["code" .= code, "flipped" .= scenarioCampaignCardFlipped a]) <$> (flip campaignCardFor (scenarioId a) =<< scenarioCampaign a)
  hideScenario = hideMeta . hideHidden . withSize "corsairDeck" "corsairDeckSize" . withSize "unchartedDeck" "unchartedDeckSize" . withSize "grottoDeck" "grottoDeckSize" . withSize "cavesDeck" "cavesDeckSize" . withSize "powerOfMordorDeck" "powerOfMordorDeckSize" . withSize "evilCreaturesDeck" "evilCreaturesDeckSize" . withSize "questDeck" "questDeckSize" . withSize "encounterDeck" "encounterDeckSize"
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
