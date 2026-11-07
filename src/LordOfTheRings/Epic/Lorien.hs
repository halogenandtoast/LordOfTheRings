module LordOfTheRings.Epic.Lorien (transferLorienSupport, receiveLorienSupport) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally (createAlly)
import LordOfTheRings.Ally.Types (AllyAttrs (..))
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (overAttrs, toAttrs, insertEntity)
import LordOfTheRings.Classes.HasQueue (push, pushWhen, runQueueT)
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Entities (alliesL, objectivesL)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Base
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Message (Message (RequestEpicEffect, CheckWindows))
import LordOfTheRings.Message.Lifted (takeControl, attachObjective)
import LordOfTheRings.Objective (createObjective)
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (InPlayArea, AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Target
import LordOfTheRings.Window (WindowType (ChangedControl), mkAfter)

transferLorienSupport :: GroupOrdinal -> PlayerId -> PlayerId -> Target -> Game -> GameT Game
transferLorienSupport destination recipient expectedController target g = do
  let own = maybe (GroupOrdinal 0) epicGroup $ scenarioEpic $ toAttrs g.scenario
      active pid = maybe False (not . (.eliminated)) $ Map.lookup pid g.players
      remotePlayer = any (\board -> board.epicBoardGroup == destination
        && any ((== recipient) . epicPlayerId) (maybe [] epicStagePlayers board.epicBoardStage))
        $ maybe [] epicBoards $ scenarioEpic $ toAttrs g.scenario
      validRecipient = if destination == own then active recipient else remotePlayer
      carried cardId effect = effect.effectTarget `elem` [target, CardIdTarget cardId]
      send packet = push $ RequestEpicEffect destination (toCardCode packet.epicLorienCard)
        (EpicReceiveLorienSupport packet recipient) ScenarioTarget
  if not validRecipient then pure g else case target of
    AllyTarget aid
      | Just host <- Map.lookup aid g.entities.allies
      , toCardCode host == toCardCode Objectives.hostOfLorien
      , InPlayArea controller <- host.placement
      , controller == expectedController
      , controller /= recipient -> do
          if destination == own
            then g <$ runQueueT (takeControl aid recipient)
            else do
              let a = toAttrs host
                  moving = carried a.allyCardId
                  packet = EpicLorienHostTransfer aid (MkEncounterCard a.allyCardId $ toCardCode host)
                    own controller a.allyOwner a.allyTokens a.allyExhausted a.allyCommitted a.allyAttacking
                    a.allyFacedown a.allyPoison a.allyUnconscious (filter moving $ gameEffects g)
              send packet
              -- Setup card: giving control does not trigger Host's leaves-play
              -- removal or any entry effects at its destination.
              pure $ g & entitiesL . alliesL %~ Map.delete aid & effectsL %~ filter (not . moving)
    ObjectiveTarget oid
      | Just power <- Map.lookup oid g.entities.objectives
      , toCardCode power == toCardCode Objectives.powerOfLorien
      , AttachedTo (PlayerTarget controller) <- power.placement
      , controller == expectedController
      , controller /= recipient -> do
          if destination == own
            then g <$ runQueueT do
              attachObjective oid $ PlayerTarget recipient
              push $ CheckWindows [mkAfter $ ChangedControl target]
            else do
              let a = toAttrs power
                  moving = carried a.cardId
                  under = [case card of PlayerCard pc -> Left pc; EncounterCard ec -> Right ec | card <- a.objectiveUnderneath]
                  packet = EpicLorienPowerTransfer oid (MkEncounterCard a.cardId $ toCardCode power)
                    own controller a.objectiveTokens a.objectiveExhausted under (filter moving $ gameEffects g)
              send packet
              pure $ g & entitiesL . objectivesL %~ Map.delete oid & effectsL %~ filter (not . moving)
    _ -> pure g

receiveLorienSupport :: EpicLorienSupportTransfer -> PlayerId -> Game -> GameT Game
receiveLorienSupport packet recipient g = do
  let own = maybe (GroupOrdinal 0) epicGroup $ scenarioEpic $ toAttrs g.scenario
      active pid = maybe False (not . (.eliminated)) $ Map.lookup pid g.players
      destination
        | active recipient = Just recipient
        | own == packet.epicLorienOrigin = find active g.playerOrder
        | otherwise = Nothing
      card = packet.epicLorienCard
  case destination of
    Nothing -> do
      -- A stale public board can name a player who has since been eliminated.
      -- Return the same physical support rather than dropping it in transit.
      push $ RequestEpicEffect packet.epicLorienOrigin (toCardCode card)
        (EpicReceiveLorienSupport packet packet.epicLorienController) ScenarioTarget
      pure g
    Just pid -> case packet of
      EpicLorienHostTransfer {} -> do
        when (Map.member packet.epicLorienAllyId g.entities.allies
          || any ((== card.id) . allyCardId . toAttrs) (toList g.entities.allies)) $
          throwIO $ userError "Epic Host of Lorien identity already exists at the destination"
        let host = overAttrs (\a -> a
              { allyPlacement = InPlayArea pid
              , allyOwner = packet.epicLorienOwner
              , allyController = Just pid
              , allyTokens = packet.epicLorienTokens
              , allyExhausted = packet.epicLorienExhausted
              , allyCommitted = packet.epicLorienCommitted
              , allyAttacking = packet.epicLorienAttacking
              , allyFacedown = packet.epicLorienFacedown
              , allyPoison = packet.epicLorienPoison
              , allyUnconscious = packet.epicLorienUnconscious
              }) $ createAlly (MkPlayerCard card.id (toCardCode card) $ Just packet.epicLorienOwner) packet.epicLorienAllyId pid
        pushWhen (own /= packet.epicLorienOrigin || pid /= packet.epicLorienController) $
          CheckWindows [mkAfter $ ChangedControl $ AllyTarget packet.epicLorienAllyId]
        pure $ g & entitiesL . alliesL %~ insertEntity host & effectsL <>~ packet.epicLorienEffects
      EpicLorienPowerTransfer {} -> do
        when (Map.member packet.epicLorienObjectiveId g.entities.objectives
          || any ((== card.id) . (.cardId) . toAttrs) (toList g.entities.objectives)) $
          throwIO $ userError "Epic Power of Lorien identity already exists at the destination"
        let power = overAttrs (\a -> a
              { objectivePlacement = AttachedTo $ PlayerTarget pid
              , objectiveTokens = packet.epicLorienTokens
              , objectiveExhausted = packet.epicLorienExhausted
              , objectiveUnderneath = map (either PlayerCard EncounterCard) packet.epicLorienUnderneath
              }) $ createObjective card packet.epicLorienObjectiveId
        pushWhen (own /= packet.epicLorienOrigin || pid /= packet.epicLorienController) $
          CheckWindows [mkAfter $ ChangedControl $ ObjectiveTarget packet.epicLorienObjectiveId]
        pure $ g & entitiesL . objectivesL %~ insertEntity power & effectsL <>~ packet.epicLorienEffects
