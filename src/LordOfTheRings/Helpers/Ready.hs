module LordOfTheRings.Helpers.Ready (readyableCards) where

import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Ally.Types (AllyAttrs (allyExhausted))
import LordOfTheRings.Hero.Types (HeroAttrs (heroExhausted))
import LordOfTheRings.Attachment.Types (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (characterController)
import LordOfTheRings.Modifier
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Target

{- | Exhausted cards an encounter-card effect can ready. The flag restricts the
choice to characters, as the Basic Swan Ship does. Player-only readying bans do
not apply to ships' encounter effects.
-}
readyableCards :: HasGame m => Bool -> m [Target]
readyableCards charactersOnly = do
  es <- getsGame gameEntities
  global <- getModifiers GameTarget
  objectives <- for (filter (.exhausted) $ toList es.objectives) \o -> do
    controller <- case o.placement of
      AttachedTo host -> characterController host
      _ -> pure $ placementController o.placement
    pure (toTarget o, controller, False)
  let characters =
        [(toTarget h, Just h.owner, True) | h <- toList es.heroes, (toAttrs h).heroExhausted]
          <> [(toTarget a, placementController a.placement, True) | a <- toList es.allies, (toAttrs a).allyExhausted]
      others =
        [(toTarget a, Just (toAttrs a).controller, False) | a <- toList es.attachments, (toAttrs a).attachmentExhausted]
          <> [(toTarget c, Just c.owner, False) | c <- toList es.contracts, c.exhausted]
          <> objectives
  fmap (map (\(target, _, _) -> target))
    $ filterM
      ( \(target, controller, character) -> do
          mods <- getModifiers target
          held <-
            maybe (pure False) (fmap (elem ControlledCardsCannotReady) . getModifiers . PlayerTarget) controller
          pure
            $ not held
            && all
              (`notElem` mods)
              [ CannotReady
              , CannotReadyFromCardEffects
              , ImmuneToCardEffects
              , ImmuneToNonMasterCardEffects
              , ConsideredOutOfPlay
              ]
            && not (character && CannotReady `elem` global)
      )
      (characters <> [candidate | not charactersOnly, candidate <- others])
