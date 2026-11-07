module LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers
  (Meta (..), emptyMeta, getMeta, cityStrength, damageDolGuldur, repairDolGuldur, epicMode, eachDolGuldurTeam, chooseDolGuldurPlayer, giveLorienSupport, chooseDolGuldurStage, siegeValue, stagePenalty, dolGuldurCaptured) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Scenario.Types (ScenarioAttrs)
import LordOfTheRings.Card (CardCode, HasCardCode, toCardCode, toCardDef, cdCardTraits)
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.I18n (textVar)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Message (Message (ContributeShared, ResolveEpicEffect, RequestEpicEffect))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Source
import LordOfTheRings.Trait (Trait (Lieutenant))
import LordOfTheRings.Target

data Meta = Meta
  { roundSiege :: Int
  , stageForcesResolved :: [CardCode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyMeta :: Meta
emptyMeta = Meta 0 []

getMeta :: ScenarioAttrs -> Meta
getMeta a = fromMaybe emptyMeta $ parseMaybe parseJSON a.meta

epicMode :: HasGame m => m Bool
epicMode = getsGame $ isJust . scenarioEpic . toAttrs . (.scenario)

cityStrength :: HasGame m => m Int
cityStrength = getsGame $ \g -> fromMaybe
  (sum [o.resources | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.dolGuldurAttack])
  (scenarioEpic (toAttrs g.scenario) >>= dolGuldurCityStrength . epicShared)

damageDolGuldur, repairDolGuldur :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
damageDolGuldur source amount = changeStrength source (negate amount)
repairDolGuldur = changeStrength

changeStrength :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
changeStrength source amount = do
  shared <- getsGame $ scenarioEpic . toAttrs . (.scenario)
  if isJust (shared >>= dolGuldurCityStrength . epicShared)
    then push $ ContributeShared DolGuldurCityStrength amount
    else do
      cities <- getsGame $ \g -> [toAttrs o | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.dolGuldurAttack]
      for_ cities $ \city ->
        if amount < 0 then dealDamage source city (negate amount) else placeResources source city amount

eachDolGuldurTeam :: ReverseQueue m => CardCode -> EpicEffect -> m ()
eachDolGuldurTeam code effect = do
  push $ ResolveEpicEffect code effect ScenarioTarget
  replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
  for_ replica $ \r ->
    for_ (nub [b.epicBoardGroup | b <- r.epicBoards, b.epicBoardGroup /= r.epicGroup]) $ \destination ->
      push $ RequestEpicEffect destination code effect ScenarioTarget

-- Only public player identities cross the team boundary; the selected player
-- resolves searches against their own private deck in their own engine.
chooseDolGuldurPlayer
  :: (ReverseQueue m, Targetable source, HasCardCode source)
  => Message -> PlayerId -> source -> EpicEffect -> m ()
chooseDolGuldurPlayer rebuild pid source effect = do
  g <- getGame
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      boards = epicBoardFor own "" g : [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
  chooseOneRebuildingM pid rebuild do
    questionLabeled "dolGuldur.choosePlayer"
    causedBy source
    for_ boards $ \board ->
      for_ (maybe [] epicStagePlayers board.epicBoardStage) $ \player ->
        labeledWith (if board.epicBoardGroup == own then "dolGuldur.localPlayer" else "dolGuldur.playerAtStage") [textVar "player" player.epicPlayerName, textVar "group" board.epicBoardName] $
          if board.epicBoardGroup == own
            then push $ ResolveEpicEffect (toCardCode source) effect $ PlayerTarget player.epicPlayerId
            else push $ RequestEpicEffect board.epicBoardGroup (toCardCode source) effect $ PlayerTarget player.epicPlayerId

-- The Epic setup card's response permits another player at any stage. Resolve
-- the transfer on the source table; that table owns the physical card to move.
giveLorienSupport
  :: (ReverseQueue m, Targetable source, HasCardCode source)
  => Message -> PlayerId -> source -> m ()
giveLorienSupport rebuild pid source = do
  g <- getGame
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      boards = epicBoardFor own "" g : [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
  chooseOneRebuildingM pid rebuild do
    questionLabeled "dolGuldur.giveSupport"
    causedBy source
    for_ boards $ \board ->
      for_ (maybe [] epicStagePlayers board.epicBoardStage) $ \player -> when (player.epicPlayerId /= pid) $
        labeledWith (if board.epicBoardGroup == own then "dolGuldur.localPlayer" else "dolGuldur.playerAtStage") [textVar "player" player.epicPlayerName, textVar "group" board.epicBoardName] $
          push $ ResolveEpicEffect (toCardCode source)
            (EpicTransferLorienSupport board.epicBoardGroup player.epicPlayerId pid) $ toTarget source

-- Attack on Dol Guldur rules p. 5: printed Siege is distinct from the
-- Siege quest keyword. These are the printed values on this encounter set.
siegeValue :: CardCode -> Int
siegeValue = \case
  "attack-on-dol-guldur-014" -> 4
  "attack-on-dol-guldur-015" -> 3
  "attack-on-dol-guldur-016" -> 2
  "attack-on-dol-guldur-017" -> 1
  "attack-on-dol-guldur-018" -> 1
  "attack-on-dol-guldur-019" -> 1
  "attack-on-dol-guldur-020" -> 2
  "attack-on-dol-guldur-021" -> 2
  "attack-on-dol-guldur-022" -> 2
  "attack-on-dol-guldur-023" -> 2
  _ -> 0

-- The unused stage has no progress, so only its mandatory fallback resolves.
-- Its physical card supplies the encounter source even while out of play.
stagePenalty :: ReverseQueue m => CardCode -> m ()
stagePenalty code = case code of
  "attack-on-dol-guldur-003" -> repairDolGuldur (CardCodeSource code) 3
  "attack-on-dol-guldur-004" -> eachDolGuldurTeam code EpicDolDiscardToOne
  "attack-on-dol-guldur-005" -> eachDolGuldurTeam code EpicDolEngageEnemies
  "attack-on-dol-guldur-006" -> eachDolGuldurTeam code EpicDolDiscardAlly
  _ -> pure ()

dolGuldurCaptured :: HasGame m => m Bool
dolGuldurCaptured = do
  strength <- cityStrength
  g <- getGame
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      localLieutenants = [() | foe <- toList g.entities.enemies, Lieutenant `member` cdCardTraits (toCardDef foe)]
      remoteLieutenants = [() | r <- maybeToList replica, board <- r.epicBoards, board.epicBoardGroup /= own
        , target <- board.epicBoardTargets, target.epicTargetKind == EpicEnemy
        , Lieutenant `member` cdCardTraits (toCardDef target.epicTargetCard)]
  pure $ strength == 0 && null localLieutenants && null remoteLieutenants

chooseDolGuldurStage
  :: (ReverseQueue m, Targetable source, HasCardCode source)
  => Message -> PlayerId -> source -> EpicEffect -> m ()
chooseDolGuldurStage rebuild pid source effect = do
  g <- getGame
  let replica = scenarioEpic $ toAttrs g.scenario
      own = maybe (GroupOrdinal 0) epicGroup replica
      boards = epicBoardFor own "" g : [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
  chooseOneRebuildingM pid rebuild do
    questionLabeled "annuminas.chooseStage"
    causedBy source
    for_ boards $ \board -> when (isJust board.epicBoardStage) $
      if board.epicBoardGroup == own
        then labeled "epic.thisGroup" $ push $ ResolveEpicEffect (toCardCode source) effect ScenarioTarget
        else labeledWith "epic.group" [textVar "group" board.epicBoardName] $
          push $ RequestEpicEffect board.epicBoardGroup (toCardCode source) effect ScenarioTarget
