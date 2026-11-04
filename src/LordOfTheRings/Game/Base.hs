{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Game.Base (module LordOfTheRings.Game.Base) where

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Location.Types (Location)
import LordOfTheRings.Card (Card)
import LordOfTheRings.Effect
import LordOfTheRings.Entities
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Log.Block
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (inEachStagingArea, isSideQuest)
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Types
import LordOfTheRings.Source (Source)
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
-}
data Separation = Separation
  { separationFirstPlayer :: PlayerId
  -- ^ who holds the first player token
  , separationAreas :: [Area]
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
isFocusedArea g a = any (`Map.member` gamePlayers g) (areaPlayers a)

-- | The players in seat order, from the one given.
seatsFrom :: PlayerId -> [PlayerId] -> [PlayerId]
seatsFrom pid order = let (before, after) = break (== pid) order in after <> before

-- | The area's first player: the first of its players in turn order from the token.
areaLead :: Game -> Separation -> Area -> PlayerId
areaLead g s a =
  fromMaybe (headEx $ areaPlayers a) $ find (`elem` areaPlayers a) (seatsFrom (separationFirstPlayer s) (gamePlayerOrder g))

-- | The areas in turn order, the first player's first.
areasInTurnOrder :: Game -> Separation -> [Area]
areasInTurnOrder g s = sortOn (\a -> elemIndex (areaLead g s a) seats) (separationAreas s)
 where
  seats = seatsFrom (separationFirstPlayer s) (gamePlayerOrder g)

-- | Bring the area holding the player into focus, holding back the one in focus.
focusArea :: PlayerId -> Game -> Game
focusArea pid g = case gameSeparation g of
  Just s
    | not (Map.member pid (gamePlayers g))
    , Just t <- find (elem pid . areaPlayers) (separationAreas s) ->
        let es = gameEntities g
            -- a quest card in each staging area comes along into focus
            shared = Map.filter inEachStagingArea (entitiesQuests es)
            hold a
              | isFocusedArea g a = a {areaHeldPlayers = gamePlayers g, areaHeldEntities = es {entitiesQuests = Map.difference (entitiesQuests es) shared}, areaHeldEffects = gameEffects g, areaHeldModifiers = gameModifiers g}
              | elem pid (areaPlayers a) = a {areaHeldPlayers = mempty, areaHeldEntities = mempty, areaHeldEffects = [], areaHeldModifiers = mempty}
              | otherwise = a
         in g
              { gamePlayers = areaHeldPlayers t
              , gameEntities = (areaHeldEntities t) {entitiesQuests = entitiesQuests (areaHeldEntities t) <> shared}
              , gameEffects = areaHeldEffects t
              , gameModifiers = areaHeldModifiers t
              , gameFirstPlayer = areaLead g s t
              , gameSeparation = Just s {separationAreas = map hold (separationAreas s)}
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
              _ -> let s' = s {separationAreas = areas} in joined {gameSeparation = Just s', gameFirstPlayer = maybe (gameFirstPlayer g) (areaLead g s') (find (isFocusedArea joined) areas)}
  _ -> g
 where
  g = focusArea target g0

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

-- | Whether no more allies can enter play this round: one has, while only one may, as
-- Corsair Waters reads.
allyEntryBarred :: Game -> Bool
allyEntryBarred g =
  OneAllyEntersPlayPerRound `elem` map modifierType (Map.findWithDefault [] GameTarget (gameModifiers g))
    && any (\e -> isAlly (effectTarget e) && EnteredPlayThisRound `elem` effectModifiers e) (gameEffects g)
 where
  isAlly = \case
    AllyTarget _ -> True
    _ -> False
