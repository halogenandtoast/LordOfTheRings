module LordOfTheRings.Objective.Cards.Derufin (derufin) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype Derufin = Derufin ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

derufin :: ObjectiveCard Derufin
derufin = objective Derufin Cards.derufin

instance HasAbilities Derufin where
  getAbilities (Derufin a) =
    [mkAbility a 1 $ forced $ TraveledToWindow After (LocationWithCardCode $ toCardCode Locations.theStoneOfErechStandalone)]

instance RunMessage Derufin where
  runMessage msg o@(Derufin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      putObjectiveAllyIntoPlay a.id =<< getLead
      pure o
    RemoveFromGame (isTarget a -> True) -> o <$ gameOver Lost
    _ -> Derufin <$> runMessage msg a
