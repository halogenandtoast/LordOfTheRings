{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Attachment.Runner (module X) where

import LordOfTheRings.Attachment.Types as X
import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasQueue (push, pushAll, pushWhen)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (characterController)
import LordOfTheRings.Matcher.Types (AttachMatcher (PlayIntoPlayArea, PlayIntoStagingArea))
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

instance RunMessage AttachmentAttrs where
  runMessage msg attrs = case msg of
    AttachCard aid host | aid == attrs.id -> do
      controller <- characterController host
      push $ CheckWindows [mkAfter $ Attached (toTarget attrs) host (cdCardTraits $ toCardDef attrs)]
      pure $ attrs & placementL .~ AttachedTo host & controllerL .~ (controller <|> attrs.attachmentController)
    -- a Trap waiting in the staging area attaches to the next enemy that can take it
    CheckWindows ws
      | attrs.placement == StagingArea
      , attrs.attachTo == PlayIntoStagingArea
      , (eid : _) <- [eid | Window After (EntersStagingArea (EnemyTarget eid)) <- ws] -> do
          eligible <- (\ms -> CannotHaveAttachments `notElem` ms && CannotHavePlayerAttachments `notElem` ms) <$> getModifiers eid
          pushWhen eligible $ AttachCard attrs.id (toTarget eid)
          pure attrs
    -- one waiting in its owner's play area, as Followed, attaches to the next enemy that
    -- engages him and can take it
    CheckWindows ws
      | InPlayArea pid <- attrs.placement
      , attrs.attachTo == PlayIntoPlayArea
      , (eid : _) <- [eid | Window After (EnemyEngaged eid pid') <- ws, pid' == pid] -> do
          eligible <- notElem CannotHaveAttachments <$> getModifiers eid
          pushWhen eligible $ AttachCard attrs.id (toTarget eid)
          pure attrs
    Exhaust (isTarget attrs -> True) -> do
      unless attrs.attachmentExhausted $ pushAll $ doFrame msg (Exhausted $ toTarget attrs)
      pure attrs
    Do (Exhaust (isTarget attrs -> True)) -> pure $ attrs & exhaustedL .~ True
    Ready (isTarget attrs -> True) -> do
      -- its controller's cards may be kept from readying, as Ungoliant's Brood's
      blocked <- elem ControlledCardsCannotReady <$> getModifiers (PlayerTarget attrs.controller)
      pure $ attrs & exhaustedL %~ (&& blocked)
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    _ -> pure attrs
