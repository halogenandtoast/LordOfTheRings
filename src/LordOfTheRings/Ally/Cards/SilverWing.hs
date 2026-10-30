module LordOfTheRings.Ally.Cards.SilverWing (silverWing) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype SilverWing = SilverWing AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

silverWing :: AllyCard SilverWing
silverWing = ally SilverWing Cards.silverWing

-- | Immune to player card effects. Each hero you control gets +1 attack.
instance HasModifiersFor SilverWing where
  getModifiersFor (SilverWing a) = do
    shipModifiers a
    for_ (shipController a) \pid -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> modified a h [AttackModifier 1]

-- | Reduce your starting threat by 3: as the fleet is prepared, it enters play.
instance HasAbilities SilverWing where
  getAbilities (SilverWing a) = [mkAbility a 1 $ silent $ EntersPlayWindow After (toTarget a)]

-- | If the Silver Wing is destroyed, its controller is eliminated from the game.
instance RunMessage SilverWing where
  runMessage msg s@(SilverWing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ (shipController a) \pid -> lowerThreat pid 3
      pure s
    _ -> SilverWing <$> runShip (traverse_ eliminatePlayer (shipController a)) msg a
