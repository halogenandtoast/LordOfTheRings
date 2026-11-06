module LordOfTheRings.Ally.Cards.GondorianWarship (gondorianWarship) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype GondorianWarship = GondorianWarship AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gondorianWarship :: AllyCard GondorianWarship
gondorianWarship = ally GondorianWarship Cards.gondorianWarship

instance HasModifiersFor GondorianWarship where
  getModifiersFor (GondorianWarship a) = shipModifiers a

instance RunMessage GondorianWarship where
  runMessage msg (GondorianWarship a) = GondorianWarship <$> runShip (setAside $ MkEncounterCard a.allyCardId $ toCardCode a) msg a
