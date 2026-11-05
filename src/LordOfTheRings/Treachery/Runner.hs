{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Treachery.Runner (module X) where

import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (CannotHaveTreacheryAttachments))
import LordOfTheRings.Message
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Source (GameSource))
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Treachery.Types as X

instance RunMessage TreacheryAttrs where
  runMessage msg attrs = case msg of
    AttachTreachery tid host | tid == attrs.treacheryId -> do
      -- a host that cannot have treachery attachments, as Beyond All Hope's hero, does not
      -- take it: it is discarded instead
      barred <- elem CannotHaveTreacheryAttachments <$> getModifiers host
      if barred
        then attrs <$ push (Discard GameSource $ toTarget attrs)
        else pure $ attrs & placementL .~ AttachedTo host
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
