{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Contract.Runner (module X) where

import LordOfTheRings.Card (cdDoubleSided, toCardDef)
import LordOfTheRings.Classes.HasQueue (pushAll)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Contract.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Window (WindowType (Exhausted))

instance RunMessage ContractAttrs where
  runMessage msg attrs = case msg of
    Flip (isTarget attrs -> True) | cdDoubleSided (toCardDef attrs) -> pure $ attrs & flippedL %~ not
    Exhaust (isTarget attrs -> True) | not attrs.exhausted -> do
      pushAll $ doFrame msg (Exhausted $ toTarget attrs)
      pure attrs
    Do (Exhaust (isTarget attrs -> True)) -> pure $ attrs & exhaustedL .~ True
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    _ -> pure attrs
