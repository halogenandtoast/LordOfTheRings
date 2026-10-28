module LordOfTheRings.Treachery.Cards.FalseLeadTheStewardsFear (falseLeadTheStewardsFear) where

import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (armFalseLead)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FalseLeadTheStewardsFear = FalseLeadTheStewardsFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

falseLeadTheStewardsFear :: TreacheryCard FalseLeadTheStewardsFear
falseLeadTheStewardsFear = treachery FalseLeadTheStewardsFear Cards.falseLeadTheStewardsFear

-- | When Revealed: End the quest phase without resolving the quest.
-- Shadow: If this attack destroys a character, remove all tokens from the current quest.
-- The scenario removes them, as a character is destroyed while the shadow lasts.
instance RunMessage FalseLeadTheStewardsFear where
  runMessage msg t@(FalseLeadTheStewardsFear attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      endQuestPhaseWithoutResolving
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      armFalseLead eid cid
      pure t
    _ -> FalseLeadTheStewardsFear <$> runMessage msg attrs
