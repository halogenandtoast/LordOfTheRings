module LordOfTheRings.Objective.Runner (module LordOfTheRings.Objective.Runner, module X) where

import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Objective.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage ObjectiveAttrs where
  runMessage msg attrs = case msg of
    ClaimObjective oid host | oid == attrs.objectiveId -> pure $ attrs & placementL .~ AttachedTo host
    PlaceObjective oid placement | oid == attrs.objectiveId -> pure $ attrs & placementL .~ placement & guardL .~ Nothing
    SetObjectiveGuard oid cid | oid == attrs.objectiveId -> pure $ attrs & guardL ?~ cid
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
