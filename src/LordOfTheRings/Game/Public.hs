module LordOfTheRings.Game.Public (publicGame, publicGameFor) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Game.Base
import LordOfTheRings.Card
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Id
import LordOfTheRings.Matcher
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
publicGameFor :: (PlayerId -> Bool) -> Game -> Value
publicGameFor seesHand g = hideDecks $ toJSON g
 where
  hideDecks = \case
    Object o ->
      Object
        $ KeyMap.insert "committedWillpower" (toJSON $ runReadGame getCommittedWillpower g)
        $ KeyMap.insert "stagingThreat" (toJSON $ runReadGame getStagingThreat g)
        $ KeyMap.insert "threatening" (toJSON [cid | (cid, n) <- runReadGame getStagingThreats g, n > 0])
        $ KeyMap.insert "stats" (toJSON $ runReadGame cardStats g)
        $ KeyMap.insert "hitPoints" (toJSON $ runReadGame hitPoints g)
        $ KeyMap.insert "attackStrength" (toJSON $ runReadGame attackStrength g)
        $ KeyMap.insert "attachmentHosts" (toJSON attachHosts)
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
      [ (c.id, runReadGame (cardHosts $ toCardCode c) g)
      | p <- toList g.players
      , seesHand p.id
      , c <- p.hand
      , c.cardType == AttachmentType
      ]
  hidePlayer = withSize "deck" "deckSize"
  hideHand = \case
    Object o | Just (Array cards) <- KeyMap.lookup "hand" o ->
      Object $ KeyMap.insert "handSize" (toJSON $ length cards) $ KeyMap.insert "hand" (Array mempty) o
    v -> v
  -- a search's cards hang off its question, which may be inside labels
  hideSearch = \case
    Object o -> Object $ adjust hideSearch "question" $ if KeyMap.member "cards" o then KeyMap.insert "cards" (Array mempty) o else o
    v -> v
  hideScenario = withSize "questDeck" "questDeckSize" . withSize "encounterDeck" "encounterDeckSize"
  withSize key sizeKey = \case
    Object o | Just (Array cards) <- KeyMap.lookup key o ->
      Object $ KeyMap.insert sizeKey (toJSON $ length cards) $ KeyMap.delete key o
    v -> v
  adjust f k o = maybe o (\v -> KeyMap.insert k (f v) o) (KeyMap.lookup k o)
  -- keyed by player
  overEntries f = \case
    Object o -> Object $ KeyMap.mapWithKey (\k -> maybe id f (parseMaybe parseJSON (String $ Key.toText k))) o
    v -> v
