module LordOfTheRings.Enemy.Cards.WolfRider (wolfRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WolfRider = WolfRider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfRider :: EnemyCard WolfRider
wolfRider = enemy WolfRider Cards.wolfRider
