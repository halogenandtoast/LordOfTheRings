{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Game.Base (module LordOfTheRings.Game.Base) where

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Attachment.Types (Attachment)
import LordOfTheRings.Location.Types (Location)
import LordOfTheRings.Card (Card)
import LordOfTheRings.Matcher.Types (cardMatches)
import LordOfTheRings.Effect
import LordOfTheRings.Entities
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Log.Block
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (inEachStagingArea, isSideQuest)
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Types
import LordOfTheRings.Source (Source (PlayerSource))
import LordOfTheRings.Target

data Game = Game
  { gamePlayers :: Map PlayerId Player
  , gamePlayerOrder :: [PlayerId]
  , gameActivePlayer :: PlayerId
  , gameFirstPlayer :: PlayerId
  , gameEntities :: Entities
  , gameScenario :: Scenario
  , gamePhase :: Phase
  , gameRound :: Int
  , gameQuestion :: Map PlayerId (Question Message)
  , gameQuestionVersion :: Int
  , gameActiveCosts :: Map CostId ActiveCost
  , gameModifiers :: Map Target [Modifier]
  , gameEffects :: [Effect]
  , gameOutcome :: Maybe Outcome
  , gameLastCommittedWillpower :: Maybe Int
  -- ^ What was committed at the last quest resolution, for the board's tally
  , gameLogBlocks :: [LogBlock]
  -- ^ The log's open blocks, outermost first. See "LordOfTheRings.Log.Block".
  , gameResolvingAttachments :: Map AttachmentId [Attachment]
  -- ^ Snapshots of activated attachments until their costs and effects finish. Nested
  -- uses form a stack; only a resolving ability, never an unrelated leaving, uses them.
  , gameLeftPlay :: Map AllyId Ally
  -- ^ Allies that just left play, kept until the window for their leaving closes so
  -- their own abilities can answer it.
  , gameLeftPlayLocations :: Map LocationId Location
  -- ^ Locations that left play this phase, so their own abilities can answer their
  -- leaving, as "after X is explored" or "after X leaves play" reads.
  , gameAttackDestroyed :: [AttackDestroyed]
  -- ^ What attacks' damage destroyed: an enemy's attack's, kept until that enemy attacks
  -- again, and a player's attack's on an enemy, until the attack has resolved.
  , gameDamageDealt :: [(Source, Target, Int)]
  -- ^ The damage each source dealt, and to what, as "each damage dealt by this attack" and
  -- "each character damaged by this effect" read: an enemy's attack's kept until that
  -- enemy attacks again, the rest until the end of the phase.
  , gameSeparation :: Maybe Separation
  -- ^ While the players are split between staging areas: see 'Separation'.
  , gameAdditionalPhases :: [Phase]
  -- ^ Additional instances of phases to come after the current one, ahead of the round's
  -- own next phase, as Send for Help adds.
  , gameResumeAfter :: Maybe Phase
  -- ^ While additional phases run, the round's own phase they followed: its next phase
  -- comes after them.
  , gameRevealer :: Maybe PlayerId
  -- ^ The player revealing encounter cards in the staging step, when each player reveals
  -- his own (The Black Riders rules insert, "New Staging Rules"): the "you" of the cards
  -- he reveals.
  , gamePeril :: Maybe PlayerId
  -- ^ The player resolving the staging of a card with the Peril keyword on his own: no
  -- other player may respond until it has resolved.
  , gameFortitudeTests :: [FortitudeTest]
  -- ^ The Fortitude tests under way, their cards discarded, the latest first.
  , gameSurgeChain :: [CardId]
  -- ^ The cards whose surge revealed another card, one after another, since the last card
  -- revealed without surge: see 'SurgeReveal'.
  }
  deriving stock (Show, Generic)

{- | Players separated into areas of their own, as at stage 4 of Foundations of Stone: each
area has its own staging area, active location and quest card, and its players act as if
they alone were in the game (cards in one area cannot affect another, except for the
uniqueness rule). The area in focus is the game itself, its players, cards and effects;
the others are held here until it is their turn to resolve the phase. Each phase is
resolved by every area in turn, from the first player's, and the first player token
still passes around the whole table; within an area its first player in turn order acts
as the first player.

Shared, as at stage 3 of Breaking of the Fellowship, the players still interact through the
normal rules of the game: only each area's stage is its own (see 'splitStage'), the
players and their cards staying in the game. The phases are played once, by every player,
each area resolving in turn what concerns its stage: questing, travel, engagement and
archery (The Road Darkens rules insert, "Multiple Staging Areas").
-}
data Separation = Separation
  { separationFirstPlayer :: PlayerId
  -- ^ who holds the first player token
  , separationAreas :: [Area]
  , separationShared :: Bool
  -- ^ the players interact, each area holding only its stage
  }
  deriving stock (Show, Generic)

data Area = Area
  { areaPlayers :: [PlayerId]
  , areaHeldPlayers :: Map PlayerId Player
  -- ^ the area's players, cards in play and effects, while another area is in focus
  , areaHeldEntities :: Entities
  , areaHeldEffects :: [Effect]
  , areaHeldModifiers :: Map Target [Modifier]
  -- ^ its cards' modifiers as they were when it was last in focus: nothing in another area
  -- changes them, so they are its cards' modifiers still
  , areaStatus :: AreaStatus
  , areaJoining :: Bool
  -- ^ its quest card defeated, its players join another area at the beginning of the
  -- travel phase
  , areaInFocus :: Bool
  -- ^ the shared stage in focus, or an empty isolated stage temporarily in focus
  }
  deriving stock (Show, Generic)

-- | How far the area is through the phase, or the end of the round, the areas resolve in turn.
data AreaStatus = AreaWaiting | AreaStarted | AreaDone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | A card an attack's damage destroyed, as "if this attack destroys a character" reads:
the attack, as its damage's source ('EnemyAttackSource', or 'PlayerAttackSource' for an
attack on an enemy), the card destroyed and who controlled it, and the card itself, for
what reads it once it has left play (Desecrated Ruins, the ally's cost). An enemy's attack
keeps the enemy's card too, for what reads the attacker once it may have left play (Battle
at the Crossings, its printed threat).
-}
data AttackDestroyed = AttackDestroyed
  { attackDestroyedBy :: Source
  , attackDestroyedTarget :: Target
  , attackDestroyedController :: Maybe PlayerId
  , attackDestroyedCard :: Card
  , attackDestroyedAttacker :: Maybe Card
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "target" AttackDestroyed Target where
  getField = attackDestroyedTarget

instance HasField "controller" AttackDestroyed (Maybe PlayerId) where
  getField = attackDestroyedController

instance HasField "card" AttackDestroyed Card where
  getField = attackDestroyedCard

makeLensesWith suffixedFields ''Game

instance ToJSON Game where
  toJSON = genericToJSON (aesonOptions $ Just "game")

instance ToJSON Separation where
  toJSON = genericToJSON (aesonOptions $ Just "separation")

instance ToJSON Area where
  toJSON = genericToJSON (aesonOptions $ Just "area")

-- | Whether the area is the one in focus: its players are the game's.
isFocusedArea :: Game -> Area -> Bool
isFocusedArea g a = case gameSeparation g of
  Just s | separationShared s -> areaInFocus a
  _ | null (areaPlayers a) -> areaInFocus a && null (gamePlayers g)
  _ -> any (`Map.member` gamePlayers g) (areaPlayers a)

-- | This isolated area's phase has already ended, as a barrow ends its player's quest.
areaPhaseEnded :: Game -> Bool
areaPhaseEnded g = (isolated g && not (any (not . (.eliminated)) (gamePlayers g))) || maybe False (any (\a -> isFocusedArea g a && areaStatus a == AreaDone) . separationAreas) (gameSeparation g)

-- | Whether the players are separated into areas of their own, apart from each other.
isolated :: Game -> Bool
isolated = maybe False (not . separationShared) . gameSeparation

-- | Whether the player's area is the one in focus, as it is when no one is separated.
inFocusedArea :: Game -> PlayerId -> Bool
inFocusedArea g pid = case gameSeparation g of
  Just s | separationShared s -> any (\a -> areaInFocus a && elem pid (areaPlayers a)) (separationAreas s)
  _ -> Map.member pid (gamePlayers g)

{- | A stage's cards, and the rest: the encounter cards in its staging area, its active
location, its quest cards but one in each staging area, and the cards attached to them.
-}
splitStage :: Entities -> (Entities, Entities)
splitStage es = splitCards roots es
 where
  atStage p = p `elem` [StagingArea, ActiveLocation]
  roots =
    [toTarget q | q <- toList es.quests, not (inEachStagingArea q)]
      <> [toTarget x | x <- toList es.enemies, atStage x.placement]
      <> [toTarget x | x <- toList es.locations, atStage x.placement]
      <> [toTarget x | x <- toList es.treacheries, atStage x.placement]
      <> [toTarget x | x <- toList es.objectives, atStage x.placement]

-- | The cards given, with what is attached to them, and the rest.
splitCards :: [Target] -> Entities -> (Entities, Entities)
splitCards roots es =
  ( Entities mempty mempty (fst enemies) (fst locations) (fst treacheries) (fst quests) (fst attachments) (fst objectives) mempty
  , Entities es.heroes es.allies (snd enemies) (snd locations) (snd treacheries) (snd quests) (snd attachments) (snd objectives) es.contracts
  )
 where
  cards = close $ Set.fromList roots
  part :: Targetable a => Map k a -> (Map k a, Map k a)
  part = Map.partition ((`Set.member` cards) . toTarget)
  enemies = part es.enemies
  locations = part es.locations
  treacheries = part es.treacheries
  quests = part es.quests
  attachments = part es.attachments
  objectives = part es.objectives
  placed =
    [(toTarget x, x.placement) | x <- toList es.enemies]
      <> [(toTarget x, x.placement) | x <- toList es.locations]
      <> [(toTarget x, x.placement) | x <- toList es.treacheries]
      <> [(toTarget x, x.placement) | x <- toList es.attachments]
      <> [(toTarget x, x.placement) | x <- toList es.objectives]
  close s =
    let s' = s <> Set.fromList [t | (t, AttachedTo h) <- placed, h `Set.member` s]
     in if s' == s then s else close s'

-- | The players in seat order, from the one given.
seatsFrom :: PlayerId -> [PlayerId] -> [PlayerId]
seatsFrom pid order = let (before, after) = break (== pid) order in after <> before

-- | The area's first player: the first of its players in turn order from the token.
areaLead :: Game -> Separation -> Area -> PlayerId
areaLead g s a =
  fromMaybe (separationFirstPlayer s) $ find (\pid -> pid `elem` areaPlayers a && living pid) (seatsFrom (separationFirstPlayer s) (gamePlayerOrder g))
 where
  living pid = maybe False (not . (.eliminated)) $ Map.lookup pid (allPlayers g)

-- | Occupied areas in turn order: an empty stage stays in play but skips phases.
areasInTurnOrder :: Game -> Separation -> [Area]
areasInTurnOrder g s = sortOn (\a -> elemIndex (areaLead g s a) seats) (filter (any living . areaPlayers) $ separationAreas s)
 where
  seats = seatsFrom (separationFirstPlayer s) (gamePlayerOrder g)
  living pid = maybe False (not . (.eliminated)) $ Map.lookup pid (allPlayers g)

-- | Bring the area holding the player into focus, holding back the one in focus.
focusArea :: PlayerId -> Game -> Game
focusArea pid g
  | maybe False (.eliminated) (Map.lookup pid $ allPlayers g) = g
  | otherwise = focusLivingArea pid g

focusLivingArea :: PlayerId -> Game -> Game
focusLivingArea pid g = case gameSeparation g of
  Just s
    | separationShared s ->
        case find (elem pid . areaPlayers) (separationAreas s) of
          Just t | not (areaInFocus t) ->
            let (stage, rest) = splitStage (gameEntities g)
                hold a
                  | areaInFocus a = a {areaHeldEntities = stage, areaHeldModifiers = gameModifiers g, areaInFocus = False}
                  | elem pid (areaPlayers a) = a {areaHeldEntities = mempty, areaHeldModifiers = mempty, areaInFocus = True}
                  | otherwise = a
             in g
                  { gameEntities = rest <> areaHeldEntities t
                  , gameModifiers = Map.union (areaHeldModifiers t) (gameModifiers g)
                  , gameSeparation = Just s {separationAreas = map hold (separationAreas s)}
                  }
          _ -> g
  _ -> focusIsolatedArea (elem pid . areaPlayers) g

-- | Bring an isolated stage into focus for a rule effect, even with nobody at it.
focusQuestArea :: QuestId -> Game -> Game
focusQuestArea qid = focusIsolatedArea (Map.member qid . entitiesQuests . areaHeldEntities)

-- | Select an isolated area by what it holds. Shared staging areas keep their own
-- focus machinery: their players and player cards never leave the current game.
focusIsolatedArea :: (Area -> Bool) -> Game -> Game
focusIsolatedArea matches g = case gameSeparation g of
  Just s
    | not (separationShared s)
    , Just (selected, t) <- find (matches . snd) (zip [0 :: Int ..] $ separationAreas s)
    , not (isFocusedArea g t) ->
        let es = gameEntities g
            -- a quest card in each staging area comes along into focus
            shared = Map.filter inEachStagingArea (entitiesQuests es)
            hold areaIndex a
              | isFocusedArea g a = a
                  { areaHeldPlayers = gamePlayers g
                  , areaHeldEntities = es {entitiesQuests = Map.difference (entitiesQuests es) shared}
                  , areaHeldEffects = gameEffects g
                  , areaHeldModifiers = gameModifiers g
                  , areaInFocus = False
                  }
              | areaIndex == selected = a
                  { areaHeldPlayers = mempty
                  , areaHeldEntities = mempty
                  , areaHeldEffects = []
                  , areaHeldModifiers = mempty
                  , areaInFocus = True
                  }
              | otherwise = a
         in g
              { gamePlayers = areaHeldPlayers t
              , gameEntities = (areaHeldEntities t) {entitiesQuests = entitiesQuests (areaHeldEntities t) <> shared}
              , gameEffects = areaHeldEffects t
              , gameModifiers = areaHeldModifiers t
              , gameFirstPlayer = areaLead g s t
              , gameSeparation = Just s {separationAreas = zipWith hold [0 ..] (separationAreas s)}
              }
  _ -> g

{- | The joining player's area joins the other's: its players, the cards they hold and
the encounter cards in its staging area go there, and its quest card leaves play. Once
one area is left the players are no longer separated.
-}
joinArea :: PlayerId -> PlayerId -> Game -> Game
joinArea joining target g0 = case gameSeparation g of
  Just s
    | Just j <- find (elem joining . areaPlayers) (separationAreas s)
    , not (isFocusedArea g j) ->
        let es = areaHeldEntities j
            areas =
              [ if isFocusedArea g a then a {areaPlayers = filter (`elem` (areaPlayers a <> areaPlayers j)) (gamePlayerOrder g)} else a
              | a <- separationAreas s
              , not (elem joining $ areaPlayers a)
              ]
            joined =
              g
                { gamePlayers = gamePlayers g <> areaHeldPlayers j
                , gameEntities = gameEntities g <> es {entitiesQuests = Map.filter isSideQuest (entitiesQuests es)}
                , gameEffects = gameEffects g <> areaHeldEffects j
                , gameModifiers = Map.unionWith (<>) (gameModifiers g) (areaHeldModifiers j)
                }
         in case areas of
              [_] -> joined {gameSeparation = Nothing, gameFirstPlayer = separationFirstPlayer s}
              _
                | separationShared s -> joined {gameSeparation = Just s {separationAreas = areas}}
                | otherwise -> let s' = s {separationAreas = areas} in joined {gameSeparation = Just s', gameFirstPlayer = maybe (gameFirstPlayer g) (areaLead g s') (find (isFocusedArea joined) areas)}
  _ -> g
 where
  g = focusArea target g0

{- | An isolated area returns to the stage identified by its quest card, even if no
players remain there. Its characters, engaged enemies and staging cards come along;
its main quest leaves play, while the destination keeps its quest and progress
(Fog on the Barrow-downs rules sheet, page 4, "Combining Staging Areas").
-}
joinQuestArea :: PlayerId -> QuestId -> Game -> Game
joinQuestArea joining qid g0 = case gameSeparation g of
  Just s
    | not (separationShared s)
    , Just destination <- find holdsQuest (separationAreas s)
    , not (isFocusedArea g destination)
    , Just departing <- find (elem joining . areaPlayers) (separationAreas s)
    , isFocusedArea g departing ->
        let players = filter (`elem` (areaPlayers destination <> areaPlayers departing)) (gamePlayerOrder g)
            destination' = destination
              { areaPlayers = players
              , areaHeldPlayers = mempty
              , areaHeldEntities = mempty
              , areaHeldEffects = []
              , areaHeldModifiers = mempty
              }
            areas =
              [ if holdsQuest a then destination' else a
              | a <- separationAreas s
              , joining `notElem` areaPlayers a
              ]
            es = gameEntities g
            joined = g
              { gamePlayers = areaHeldPlayers destination <> gamePlayers g
              , gameEntities = areaHeldEntities destination <> es {entitiesQuests = Map.filter isSideQuest (entitiesQuests es)}
              , gameEffects = areaHeldEffects destination <> gameEffects g
              , gameModifiers = Map.unionWith (<>) (areaHeldModifiers destination) (gameModifiers g)
              }
         in case areas of
              [_] -> joined {gameSeparation = Nothing, gameFirstPlayer = separationFirstPlayer s}
              _ -> joined
                { gameSeparation = Just s {separationAreas = areas}
                , gameFirstPlayer = areaLead joined s destination'
                }
  _ -> g0
 where
  g = focusArea joining g0
  holdsQuest = Map.member qid . entitiesQuests . areaHeldEntities

-- | Every player, those in areas not in focus too.
allPlayers :: Game -> Map PlayerId Player
allPlayers g = gamePlayers g <> foldMap (foldMap areaHeldPlayers . separationAreas) (gameSeparation g)

-- | The cards in play in the areas not in focus.
heldEntities :: Game -> [Entities]
heldEntities g = maybe [] (map areaHeldEntities . separationAreas) (gameSeparation g)

instance HasField "players" Game (Map PlayerId Player) where
  getField = gamePlayers

instance HasField "playerOrder" Game [PlayerId] where
  getField = gamePlayerOrder

instance HasField "activePlayer" Game PlayerId where
  getField = gameActivePlayer

instance HasField "firstPlayer" Game PlayerId where
  getField = gameFirstPlayer

instance HasField "entities" Game Entities where
  getField = gameEntities

instance HasField "scenario" Game Scenario where
  getField = gameScenario

instance HasField "phase" Game Phase where
  getField = gamePhase

instance HasField "round" Game Int where
  getField = gameRound

instance HasField "question" Game (Map PlayerId (Question Message)) where
  getField = gameQuestion

instance HasField "questionVersion" Game Int where
  getField = gameQuestionVersion

instance HasField "outcome" Game (Maybe Outcome) where
  getField = gameOutcome

-- | Whether the player cannot put the card into play under his control, as Fellowship
-- bars non-unique allies.
barredFromPlay :: Game -> PlayerId -> Card -> Bool
barredFromPlay g pid card =
  or [cardMatches m card | CannotPutIntoPlay m <- map modifierType (Map.findWithDefault [] (PlayerTarget pid) (gameModifiers g))]

-- | Whether no more allies can enter play under the player this round: none may at all, as
-- The Crack of Doom reads, or one has, while only one may, as Corsair Waters reads of the
-- game, and The Spider's Pass of each player ("each player cannot play, or put into play,
-- more than 1 ally each round").
allyEntryBarred :: PlayerId -> Game -> Bool
allyEntryBarred pid g =
  AlliesCannotEnterPlay `elem` gameMods
    || (on GameTarget && notNull entered)
    || (on (PlayerTarget pid) && any (== PlayerSource pid) entered)
 where
  gameMods = map modifierType (Map.findWithDefault [] GameTarget (gameModifiers g))
  on t = OneAllyEntersPlayPerRound `elem` map modifierType (Map.findWithDefault [] t (gameModifiers g))
  -- the player an ally entered play under is the source of its mark
  entered = [effectSource e | e <- gameEffects g, isAlly (effectTarget e), EnteredPlayThisRound `elem` effectModifiers e]
  isAlly = \case
    AllyTarget _ -> True
    _ -> False

-- | Plays and puts into play share the limit, including earlier actions in a round
-- before a stage imposing it entered play. Replaying the same card counts again.
handEntryLimitReached :: Game -> PlayerId -> Bool
handEntryLimitReached g pid = any (used >=) limits
 where
  mods = map modifierType $ Map.findWithDefault [] (PlayerTarget pid) (gameModifiers g)
  limits = [n | CardsFromHandPerRoundLimit n <- mods]
  used = length [() | Effect _ (PlayerTarget p) ms _ <- gameEffects g, p == pid, CardFromHandThisRound _ <- ms]
