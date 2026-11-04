module LordOfTheRings.Enemy.Cards.HaradTiger (haradTiger) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype HaradTiger = HaradTiger EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradTiger :: EnemyCard HaradTiger
haradTiger = enemy HaradTiger Cards.haradTiger

-- | Cannot be optionally engaged.
instance HasModifiersFor HaradTiger where
  getModifiersFor (HaradTiger a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]
