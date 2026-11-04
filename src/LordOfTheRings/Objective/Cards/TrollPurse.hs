module LordOfTheRings.Objective.Cards.TrollPurse (trollPurse) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (attachToATroll, runTrollTreasure, trollTreasureAbilities)

newtype TrollPurse = TrollPurse ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollPurse :: ObjectiveCard TrollPurse
trollPurse = objective TrollPurse Cards.trollPurse

-- | If Troll Purse is discarded, add it to the staging area.
-- If Troll Purse is unattached and in the staging area, attach it to a Troll enemy, if able.
instance HasAbilities TrollPurse where
  getAbilities (TrollPurse a) = trollTreasureAbilities a

{- | Response: After attached Troll enemy is destroyed, the first player may spend 1 Baggins
resource to claim this objective and attach it to Bilbo Baggins.

As its Troll is destroyed the purse drops into the staging area, but is not attached to
another Troll until the first player has had the chance to claim it.
-}
instance RunMessage TrollPurse where
  runMessage msg o@(TrollPurse attrs) = case msg of
    DoStep 1 (Destroy _ host@(EnemyTarget _)) | attrs.placement == AttachedTo host -> do
      lead <- getLead
      bilbo <- getBilbo
      resources <- getBagginsResources
      case bilbo of
        Just b | resources > 0 -> chooseOneM lead do
          questionLabeled "trollPurse.claim"
          causedBy attrs
          labeled "trollPurse.spend" do
            spendResourcesFrom lead b 1
            attachObjective attrs.id b
          labeled "pass" unclaimed
        _ -> unclaimed
      pure $ TrollPurse $ attrs & placementL .~ StagingArea
    -- unclaimed, it is attached to a Troll enemy in play, if able
    DoStep 1 (PlaceObjective oid StagingArea) | oid == attrs.id -> do
      when (attrs.placement == StagingArea) $ attachToATroll attrs
      pure o
    _ -> TrollPurse <$> runTrollTreasure msg attrs
   where
    unclaimed :: ReverseQueue m => m ()
    unclaimed = push $ DoStep 1 (PlaceObjective attrs.id StagingArea)
