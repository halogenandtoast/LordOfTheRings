module LordOfTheRings.Attachment.Runner (module LordOfTheRings.Attachment.Runner, module X) where

import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Attachment.Types as X
import LordOfTheRings.Classes.HasQueue (pushWhen)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher.Types (AttachMatcher (PlayIntoStagingArea))
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Window
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage AttachmentAttrs where
  runMessage msg attrs = case msg of
    AttachCard aid host | aid == attrs.id -> pure $ attrs & placementL .~ AttachedTo host
    -- a Trap waiting in the staging area attaches to the next enemy that can take it
    CheckWindows ws
      | attrs.placement == StagingArea
      , attrs.attachTo == PlayIntoStagingArea
      , (eid : _) <- [eid | Window After (EntersStagingArea (EnemyTarget eid)) <- ws] -> do
          eligible <- notElem CannotHaveAttachments <$> getModifiers eid
          pushWhen eligible $ AttachCard attrs.id (toTarget eid)
          pure attrs
    Exhaust (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
