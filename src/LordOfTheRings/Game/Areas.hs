module LordOfTheRings.Game.Areas (module LordOfTheRings.Game.Areas) where

import Data.Map.Strict qualified as Map (filterWithKey, lookup, member)
import LordOfTheRings.Attachment (
  AttachmentAttrs (attachmentOwner),
 )
import LordOfTheRings.Classes.Entity (Entity (toAttrs))
import LordOfTheRings.Effect (Effect (effectTarget))
import LordOfTheRings.Entities (
  Entities (
    Entities,
    entitiesAllies,
    entitiesAttachments,
    entitiesContracts,
    entitiesEnemies,
    entitiesHeroes,
    entitiesLocations,
    entitiesObjectives,
    entitiesQuests,
    entitiesTreacheries
  ),
 )
import LordOfTheRings.Game.Base (
  Area (..),
  AreaStatus (..),
  Game (
    gameEffects,
    gameEntities,
    gameModifiers,
    gamePlayers,
    gameSeparation
  ),
  Separation (..),
  areaLead,
  areasInTurnOrder,
  focusArea,
  isFocusedArea,
  seatsFrom,
 )
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Message (Message (FocusArea, Run))
import LordOfTheRings.Placement (
  Placement (..),
  placementController,
 )
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest (QuestAttrs (questOwner, questPlayedBy))
import LordOfTheRings.Target (
  Target (
    AllyTarget,
    AttachmentTarget,
    ContractTarget,
    EnemyTarget,
    GameTarget,
    HeroTarget,
    LocationTarget,
    ObjectiveTarget,
    PlayerTarget,
    QuestTarget,
    TreacheryTarget
  ),
  Targetable (toTarget),
 )

{- | The players split into areas, one for each group: each player takes his cards in
play, those engaged with him and those attached to them, and the encounter cards no
player holds stay with the first group. The effects on a player or a card go with it,
and those on the game apply in every area. The areas are separated during a phase, so
each has begun it, and the first player's is in focus.
-}
separate :: [[PlayerId]] -> Game -> Game
separate groups g
  | isJust (gameSeparation g) || length groups < 2 = g
  | otherwise =
      focusArea g.firstPlayer
        g
          { gamePlayers = mempty
          , gameEntities = mempty
          , gameEffects = []
          , gameSeparation = Just $ Separation g.firstPlayer (zipWith area [0 ..] groups) False
          }
 where
  es = g.entities
  area :: Int -> [PlayerId] -> Area
  area i pids =
    Area
      { areaPlayers = pids
      , areaHeldPlayers = Map.filterWithKey (\k _ -> k `elem` pids) g.players
      , areaHeldEntities =
          Entities
            { entitiesHeroes = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.heroes
            , entitiesAllies = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.allies
            , entitiesEnemies = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.enemies
            , entitiesLocations = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.locations
            , entitiesTreacheries = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.treacheries
            , entitiesQuests = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.quests
            , entitiesAttachments = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.attachments
            , entitiesObjectives = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.objectives
            , entitiesContracts = Map.filterWithKey (\k _ -> inGroup (toTarget k)) es.contracts
            }
      , areaHeldEffects = filter (\e -> maybe True (== i) (effectGroup e)) (gameEffects g)
      , areaHeldModifiers = gameModifiers g
      , areaStatus = AreaStarted
      , areaJoining = False
      , areaInFocus = False
      }
   where
    inGroup t = groupOf t == i
  groupOf t = case holderOf t of
    Just pid | Just i <- findIndex (elem pid) groups -> i
    _ -> 0
  effectGroup e = case effectTarget e of
    GameTarget -> Nothing
    t -> Just (groupOf t)
  holderOf :: Target -> Maybe PlayerId
  holderOf = \case
    PlayerTarget pid -> Just pid
    HeroTarget k -> (.owner) <$> Map.lookup k es.heroes
    AllyTarget k -> placed . (.placement) =<< Map.lookup k es.allies
    EnemyTarget k -> placed . (.placement) =<< Map.lookup k es.enemies
    LocationTarget k -> placed . (.placement) =<< Map.lookup k es.locations
    TreacheryTarget k -> placed . (.placement) =<< Map.lookup k es.treacheries
    ObjectiveTarget k -> placed . (.placement) =<< Map.lookup k es.objectives
    ContractTarget k -> (.owner) <$> Map.lookup k es.contracts
    -- a Trap in the staging area follows its owner (ALeP 3.382)
    AttachmentTarget k -> Map.lookup k es.attachments <&> \a -> fromMaybe (attachmentOwner $ toAttrs a) (placed a.placement)
    QuestTarget k -> (\q -> questPlayedBy (toAttrs q) <|> questOwner (toAttrs q)) =<< Map.lookup k es.quests
    _ -> Nothing
  placed = \case
    AttachedTo t -> holderOf t
    p -> placementController p

{- | Split one player from the current isolated area, retaining its stage even when
no players remain there (Fog on the Barrow-downs). Existing areas keep their state;
the departing player takes only the cards held by him and starts with no stage.
-}
separatePlayer :: PlayerId -> Game -> Game
separatePlayer pid g
  | not (Map.member pid g.players) = g
  | maybe False separationShared (gameSeparation g) = g
  | otherwise = case gameSeparation divided of
      Nothing -> g
      Just fresh -> divided {gameSeparation = Just $ case gameSeparation g of
        Nothing -> fresh
        Just old -> old {separationAreas = concatMap replace (separationAreas old)}
          where
            replace a
              | isFocusedArea g a = case separationAreas fresh of
                  home : away -> home {areaStatus = areaStatus a, areaJoining = areaJoining a} : away
                  [] -> []
              | otherwise = [a]
        }
 where
  others = [p | p <- g.playerOrder, p /= pid, Map.member p g.players]
  divided = focusArea pid $ separate [others, [pid]] g {gameSeparation = Nothing}

{- | The players split into areas sharing the game, one for each group: the first player's
group takes the stage in play, each other group's starts empty. See 'Separation'.
-}
separateStages :: [[PlayerId]] -> Game -> Game
separateStages groups g
  | isJust (gameSeparation g) || length groups < 2 = g
  | otherwise = g {gameSeparation = Just $ Separation g.firstPlayer (map area groups) True}
 where
  area pids =
    Area
      { areaPlayers = pids
      , areaHeldPlayers = mempty
      , areaHeldEntities = mempty
      , areaHeldEffects = []
      , areaHeldModifiers = mempty
      , areaStatus = AreaStarted
      , areaJoining = False
      , areaInFocus = g.firstPlayer `elem` pids
      }

{- | Sharing the game split between staging areas, each area's first player and its
players, in turn order: see 'Separation'.
-}
stageGroups :: Game -> Maybe [(PlayerId, [PlayerId])]
stageGroups g = case gameSeparation g of
  Just s | separationShared s -> Just [(areaLead g s a, filter (`elem` areaPlayers a) seats) | a <- areasInTurnOrder g s]
   where
    seats = seatsFrom (separationFirstPlayer s) g.playerOrder
  _ -> Nothing

{- | While the players are separated each area resolves the step in turn, in turn order:
what follows the step in the area in focus is the step again in the next area still to
resolve it (the messages given, from how far that area is), and once every area has,
what follows it, back with the first player.
-}
eachAreaThen :: (AreaStatus -> [Message]) -> (Separation -> Separation) -> Message -> Game -> (Message, Game)
eachAreaThen again finish next g = case gameSeparation g of
  Just s0 | not (separationShared s0) ->
    let s = s0 {separationAreas = [if isFocusedArea g a then a {areaStatus = AreaDone} else a | a <- separationAreas s0]}
     in case [a | a <- areasInTurnOrder g s, areaStatus a /= AreaDone] of
          (a : _) -> (Run (FocusArea (areaLead g s a) : again (areaStatus a)), g {gameSeparation = Just s})
          [] ->
            let s' = finish s {separationAreas = [a {areaStatus = AreaWaiting} | a <- separationAreas s]}
             in (Run [FocusArea (separationFirstPlayer s'), next], g {gameSeparation = Just s'})
  -- sharing the game, the areas resolve their stages in turn within the phase
  _ -> (next, g)

{- | Run the message on one thing in play, putting what it pushes on the queue once it is
done.
-}
