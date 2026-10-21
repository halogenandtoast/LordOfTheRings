module LordOfTheRings.Enemy.Cards.Stuart (stuart) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Troll))

newtype Stuart = Stuart EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

stuart :: EnemyCard Stuart
stuart = enemy Stuart Cards.stuart

-- | While Stuart is engaged with a player, all Troll enemies get +1 defense.
instance HasModifiersFor Stuart where
  getModifiersFor (Stuart a) = case a.placement of
    EngagedWith _ -> traverse_ (\t -> modified a t [DefenseModifier 1]) =<< select (EnemyWithTrait Troll)
    _ -> pure ()
