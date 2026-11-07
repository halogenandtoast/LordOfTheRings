module LordOfTheRings.Game.Setup (module LordOfTheRings.Game.Setup) where

import Data.Map.Strict qualified as Map (fromList)
import LordOfTheRings.Classes.Entity (Entity (toId))
import LordOfTheRings.Contract (createContract)
import LordOfTheRings.Entities (
  Entities (entitiesContracts, entitiesHeroes),
 )
import LordOfTheRings.Game.Base (Game (..))
import LordOfTheRings.Hero (Hero)
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Player.Runner (Player)
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Scenario (Scenario, passageThroughMirkwood)

newGame :: Player -> [Hero] -> Game
newGame = newGameIn passageThroughMirkwood

newGameIn :: Scenario -> Player -> [Hero] -> Game
newGameIn gameScenario player heroes = newTableIn gameScenario ((player, heroes) :| [])

-- | A game with a seat for each player, in the order given.
newTableIn :: Scenario -> NonEmpty (Player, [Hero]) -> Game
newTableIn gameScenario seats@((player, _) :| _) =
  Game
    { gamePlayers = Map.fromList [(p.id, p) | (p, _) <- toList seats]
    , gamePlayerOrder = [p.id | (p, _) <- toList seats]
    , gameActivePlayer = player.id
    , gameFirstPlayer = player.id
    , gameEntities =
        mempty
          { entitiesHeroes = Map.fromList [(h.id, h) | (_, hs) <- toList seats, h <- hs]
          , -- each player's contract is put into play with his heroes (MEC77 rules)
            entitiesContracts = Map.fromList [(toId c, c) | (p, _) <- toList seats, Just card <- [p.contract], let c = createContract p.id card]
          }
    , gameScenario
    , gamePhase = ResourcePhase
    , gameRound = 0
    , gameQuestion = mempty
    , gameQuestionVersion = 0
    , gameActiveCosts = mempty
    , gameModifiers = mempty
    , gameEffects = []
    , gameFrozenRoundEndAbilities = Nothing
    , gameOutcome = Nothing
    , gameLastCommittedWillpower = Nothing
    , gameLogBlocks = []
    , gameResolvingAttachments = mempty
    , gameResolvingObjectives = mempty
    , gameLeftPlay = mempty
    , gameLeftPlayLocations = mempty
    , gameAttackDestroyed = []
    , gameDamageDealt = []
    , gameSeparation = Nothing
    , gameAdditionalPhases = []
    , gameResumeAfter = Nothing
    , gameRevealer = Nothing
    , gamePeril = Nothing
    , gameFortitudeTests = []
    , gameSurgeChain = []
    }

