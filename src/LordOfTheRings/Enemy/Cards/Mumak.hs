module LordOfTheRings.Enemy.Cards.Mumak (mumak) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Mumak = Mumak EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mumak :: EnemyCard Mumak
mumak = enemy Mumak Cards.mumak

-- | No attachments can be attached to Mumak.
-- Mumak cannot take more than 3 damage each round.
instance HasModifiersFor Mumak where
  getModifiersFor (Mumak a) = modified a a [CannotHaveAttachments, DamageLimit 3]
