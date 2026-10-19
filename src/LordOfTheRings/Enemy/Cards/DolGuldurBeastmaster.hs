module LordOfTheRings.Enemy.Cards.DolGuldurBeastmaster (dolGuldurBeastmaster) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DolGuldurBeastmaster = DolGuldurBeastmaster EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dolGuldurBeastmaster :: EnemyCard DolGuldurBeastmaster
dolGuldurBeastmaster = enemy DolGuldurBeastmaster Cards.dolGuldurBeastmaster
