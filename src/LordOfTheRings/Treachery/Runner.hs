{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Treachery.Runner (module X) where

import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Message
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Treachery.Types as X

instance RunMessage TreacheryAttrs where
  runMessage msg attrs = case msg of
    AttachTreachery tid host | tid == attrs.treacheryId -> pure $ attrs & placementL .~ AttachedTo host
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
