module LordOfTheRings.Treachery.Cards.FalseLeadTheStewardsFear (falseLeadTheStewardsFear) where

import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (removeAllTokensFromQuest)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FalseLeadTheStewardsFear = FalseLeadTheStewardsFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

falseLeadTheStewardsFear :: TreacheryCard FalseLeadTheStewardsFear
falseLeadTheStewardsFear = treachery FalseLeadTheStewardsFear Cards.falseLeadTheStewardsFear

-- | When Revealed: End the quest phase without resolving the quest.
-- Shadow: If this attack destroys a character, remove all tokens from the current quest.
instance RunMessage FalseLeadTheStewardsFear where
  runMessage msg t@(FalseLeadTheStewardsFear attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      endQuestPhaseWithoutResolving
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      removeAllTokensFromQuest (CardIdSource cid)
      pure t
    _ -> FalseLeadTheStewardsFear <$> runMessage msg attrs
