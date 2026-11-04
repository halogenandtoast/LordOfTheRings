module LordOfTheRings.Ally.Cards.ForestRoadTraveler (forestRoadTraveler) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype ForestRoadTraveler = ForestRoadTraveler AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestRoadTraveler :: AllyCard ForestRoadTraveler
forestRoadTraveler = ally ForestRoadTraveler Cards.forestRoadTraveler

-- | Forest Road Traveler gets +1 [willpower], +1 [attack], and +1 [defense] for each
-- attachment on the active location.
instance HasModifiersFor ForestRoadTraveler where
  getModifiersFor (ForestRoadTraveler a) = do
    n <- activeLocationAttachments
    when (n > 0) $ modifySelf a [WillpowerModifier n, AttackModifier n, DefenseModifier n]

instance RunMessage ForestRoadTraveler where
  runMessage msg (ForestRoadTraveler a) = ForestRoadTraveler <$> runMessage msg a
