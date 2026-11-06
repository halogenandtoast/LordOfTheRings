module LordOfTheRings.Objective.Cards.GondorianWarship (gondorianWarshipObjective) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype GondorianWarship = GondorianWarship ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gondorianWarshipObjective :: ObjectiveCard GondorianWarship
gondorianWarshipObjective = objective GondorianWarship Cards.gondorianWarship

instance HasModifiersFor GondorianWarship where
  getModifiersFor (GondorianWarship a) = modifySelf a [ImmuneToPlayerCardEffects]

instance RunMessage GondorianWarship where
  runMessage msg o@(GondorianWarship attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      players <- getRemainingPlayers
      chooseOneM lead do
        questionLabeled "dreadnaught.warshipController"
        causedBy attrs
        targets players \pid -> push $ PutObjectiveAllyIntoPlay attrs.id pid
      pure o
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dealDamage (CardCodeSource $ toCardCode attrs) eid 5
      pure o
    _ -> GondorianWarship <$> runMessage msg attrs
