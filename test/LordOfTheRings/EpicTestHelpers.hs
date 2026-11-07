module LordOfTheRings.EpicTestHelpers (commandEffect, acknowledgeEpicCommand) where

import LordOfTheRings.Epic.Types
import TestImport

-- Assertions about a printed cross-team effect need not depend on the durable
-- request envelope. Protocol/three-team tests exercise the envelope itself.
commandEffect :: EpicCommand -> EpicEffect
commandEffect command = unwrap command.epicCommandEffect
 where
  unwrap (EpicResolveWithReply _ _ effect) = unwrap effect
  unwrap effect = effect

-- A source-only fixture can explicitly simulate the recipient finishing a
-- request before inspecting the source's next queued request.
acknowledgeEpicCommand :: EpicCommand -> GameT ()
acknowledgeEpicCommand command = case command.epicCommandEffect of
  EpicResolveWithReply _ nonce _ ->
    void $ runDebug $ ResolveEpicEffect command.epicCommandCard (EpicAcknowledge nonce) ScenarioTarget
  _ -> error "Epic command has no acknowledgement"
