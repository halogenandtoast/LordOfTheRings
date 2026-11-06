module LordOfTheRings.Helpers.Resources (discardableResources) where

import LordOfTheRings.Ally.Types (AllyAttrs (allyTokens))
import LordOfTheRings.Attachment.Types (AttachmentAttrs (attachmentTokens))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Hero.Types (HeroAttrs (heroTokens, heroPrisoner, heroInStagingArea, heroGuardedBy))
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Location.Types (LocationAttrs (locationTokens))
import LordOfTheRings.Matcher (characterController)
import LordOfTheRings.Modifier (ModifierType (ConsideredOutOfPlay))
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveTokens))
import LordOfTheRings.Placement (Placement (..), placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs (questTokens, questOwner))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Treachery.Types (TreacheryAttrs (treacheryTokens))

-- | Resources in the current staging area, with their controllers. Encounter
-- cards do not become player-controlled merely by engaging or attaching to them.
-- Time counters and villagers use resource tokens physically, but are not resources.
discardableResources :: HasGame m => Source -> m [(Target, Maybe PlayerId, Int)]
discardableResources source = do
  es <- getsGame gameEntities
  objectives <- for (filter (inPlay . (.placement)) $ toList es.objectives) \o -> do
    controller <- case o.placement of
      AttachedTo host -> characterController host
      InPlayArea pid -> pure $ Just pid
      _ -> pure Nothing
    pure (toTarget o, controller, objectiveTokens $ toAttrs o)
  let pools =
        [(toTarget h, if (toAttrs h).heroInStagingArea || isJust (toAttrs h).heroGuardedBy then Nothing else Just h.owner, heroTokens $ toAttrs h) | h <- toList es.heroes, not (toAttrs h).heroPrisoner]
          <> [(toTarget a, placementController a.placement, allyTokens $ toAttrs a) | a <- toList es.allies, inPlay a.placement]
          <> [(toTarget a, Just (toAttrs a).controller, attachmentTokens $ toAttrs a) | a <- toList es.attachments, inPlay a.placement]
          <> [(toTarget e, Nothing, enemyTokens $ toAttrs e) | e <- toList es.enemies, inPlay e.placement]
          <> [(toTarget l, Nothing, locationTokens $ toAttrs l) | l <- toList es.locations, inPlay l.placement]
          <> [(toTarget t, Nothing, treacheryTokens $ toAttrs t) | t <- toList es.treacheries, inPlay t.placement]
          <> [(toTarget q, (toAttrs q).questOwner, questTokens $ toAttrs q) | q <- toList es.quests]
          <> objectives
  filterM (\(target, _, _) -> andM [not <$> immuneToSource source target, notElem ConsideredOutOfPlay <$> getModifiers target])
    [(target, controller, n) | (target, controller, tokens) <- pools, let n = countTokens Resource tokens, n > 0]
 where
  inPlay placement = placement `notElem` [Unplaced, InVictoryDisplay]
