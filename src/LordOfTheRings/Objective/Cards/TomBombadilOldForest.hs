module LordOfTheRings.Objective.Cards.TomBombadilOldForest (tomBombadilOldForest) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype TomBombadilOldForest = TomBombadilOldForest ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

tomBombadilOldForest :: ObjectiveCard TomBombadilOldForest
tomBombadilOldForest = objective TomBombadilOldForest Cards.tomBombadilOldForest

instance HasModifiersFor TomBombadilOldForest where
  getModifiersFor (TomBombadilOldForest a) = modifySelf a [WhenRevealedCannotBeCanceled]

instance RunMessage TomBombadilOldForest where
  runMessage msg o@(TomBombadilOldForest a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      revealer <- getRevealer
      choosePlayerM revealer a \pid -> do
        putObjectiveAllyIntoPlayReadyAndCommitted a.id pid
        roundModifier a GameTarget $ DiscardAtEndOfRoundThenShuffleEncounterDiscard a.cardId
      pure o
    _ -> TomBombadilOldForest <$> runMessage msg a
