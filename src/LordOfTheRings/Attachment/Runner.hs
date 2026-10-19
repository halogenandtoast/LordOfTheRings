module LordOfTheRings.Attachment.Runner (module LordOfTheRings.Attachment.Runner, module X) where

import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Attachment.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage AttachmentAttrs where
  runMessage msg attrs = case msg of
    AttachCard aid host | aid == attrs.id -> pure $ attrs & placementL .~ AttachedTo host
    Exhaust (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
