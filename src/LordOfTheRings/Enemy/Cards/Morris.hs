module LordOfTheRings.Enemy.Cards.Morris (morris) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Troll))

newtype Morris = Morris EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

morris :: EnemyCard Morris
morris = enemy Morris Cards.morris

-- | While Morris is engaged with a player, all Troll enemies get +1 attack.
instance HasModifiersFor Morris where
  getModifiersFor (Morris a) = case a.placement of
    EngagedWith _ -> traverse_ (\t -> modified a t [AttackModifier 1]) =<< select (EnemyWithTrait Troll)
    _ -> pure ()
