module LordOfTheRings.Location.Cards.CrewQuarters (crewQuarters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype CrewQuarters = CrewQuarters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crewQuarters :: LocationCard CrewQuarters
crewQuarters = location CrewQuarters Cards.crewQuarters

-- | Travel: Add 1 resource to each Raider card in play to travel here.
-- Shadow: Attacking enemy gets +2 attack.
instance RunMessage CrewQuarters where
  runMessage msg l@(CrewQuarters attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      enemies <- select (EnemyWithTrait Raider)
      for_ enemies \e -> placeResources attrs e 1
      -- and Captain Sahír and Na'asiyah on their objective sides
      allies <- select (AllyWithTrait Raider)
      for_ allies \a -> placeResources attrs a 1
      CrewQuarters <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      attackModifier attrs eid (AttackModifier 2)
      pure l
    _ -> CrewQuarters <$> runMessage msg attrs
