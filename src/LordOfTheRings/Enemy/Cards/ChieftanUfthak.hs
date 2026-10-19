module LordOfTheRings.Enemy.Cards.ChieftanUfthak (chieftanUfthak) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ChieftanUfthak = ChieftanUfthak EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

chieftanUfthak :: EnemyCard ChieftanUfthak
chieftanUfthak = enemy ChieftanUfthak Cards.chieftanUfthak
