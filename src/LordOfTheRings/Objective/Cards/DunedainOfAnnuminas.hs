module LordOfTheRings.Objective.Cards.DunedainOfAnnuminas (dunedainOfAnnuminas) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype DunedainOfAnnuminas = DunedainOfAnnuminas ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainOfAnnuminas :: ObjectiveCard DunedainOfAnnuminas
dunedainOfAnnuminas = objective DunedainOfAnnuminas Cards.dunedainOfAnnuminas

instance RunMessage DunedainOfAnnuminas where
  runMessage msg o@(DunedainOfAnnuminas a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      players <- filterM (fmap (> 0) . (`getResourcesOfSphere` Neutral)) =<< getRemainingPlayers
      lead <- getLead
      if null players
        then discard a a
        else chooseOneM lead do
          questionLabeled "annuminas.dunedain"
          causedBy a
          targets players $ \pid -> do
            spendResources pid Neutral 1
            putObjectiveAllyIntoPlay a.id pid
          labeled "annuminas.declineDunedain" $ discard a a
      pure o
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders $ \defender -> do
        encounterReadies defender
        attackModifier a defender (DefenseModifier 2)
      pure o
    _ -> DunedainOfAnnuminas <$> runMessage msg a
