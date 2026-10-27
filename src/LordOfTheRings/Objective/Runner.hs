{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Objective.Runner (module X) where

import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Message
import LordOfTheRings.Objective.Types as X
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (WindowType (Exhausted), mkAfter)

instance RunMessage ObjectiveAttrs where
  runMessage msg attrs = case msg of
    ClaimObjective oid host | oid == attrs.objectiveId -> pure $ attrs & placementL .~ AttachedTo host
    PlaceObjective oid placement | oid == attrs.objectiveId -> pure $ attrs & placementL .~ placement & guardL .~ Nothing
    SetObjectiveGuard oid cid | oid == attrs.objectiveId -> pure $ attrs & guardL ?~ cid
    Exhaust (isTarget attrs -> True) | not attrs.exhausted -> do
      push $ CheckWindows [mkAfter $ Exhausted (toTarget attrs)]
      pure $ attrs & exhaustedL .~ True
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
