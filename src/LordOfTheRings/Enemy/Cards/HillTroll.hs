module LordOfTheRings.Enemy.Cards.HillTroll (hillTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype HillTroll = HillTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hillTroll :: EnemyCard HillTroll
hillTroll = enemy HillTroll Cards.hillTroll
