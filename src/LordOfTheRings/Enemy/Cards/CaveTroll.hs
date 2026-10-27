module LordOfTheRings.Enemy.Cards.CaveTroll (caveTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CaveTroll = CaveTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveTroll :: EnemyCard CaveTroll
caveTroll = enemy CaveTroll Cards.caveTroll

-- | For each excess point of combat damage dealt by Cave-troll (damage that is dealt beyond
-- the remaining hit points of the character damaged by its attack) you must damage another
-- character you control.
instance HasModifiersFor CaveTroll where
  getModifiersFor (CaveTroll a) = modifySelf a [ExcessDamageToOtherCharacters]
