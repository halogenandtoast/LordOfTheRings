module LordOfTheRings.Enemy.Cards.DolGuldurOrcs (dolGuldurOrcs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DolGuldurOrcs = DolGuldurOrcs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dolGuldurOrcs :: EnemyCard DolGuldurOrcs
dolGuldurOrcs = enemy DolGuldurOrcs Cards.dolGuldurOrcs
