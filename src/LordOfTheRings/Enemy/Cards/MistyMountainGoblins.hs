module LordOfTheRings.Enemy.Cards.MistyMountainGoblins (mistyMountainGoblins) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MistyMountainGoblins = MistyMountainGoblins EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mistyMountainGoblins :: EnemyCard MistyMountainGoblins
mistyMountainGoblins = enemy MistyMountainGoblins Cards.mistyMountainGoblins
