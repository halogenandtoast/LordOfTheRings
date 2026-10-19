module LordOfTheRings.Enemy.Cards.HillTroll (hillTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype HillTroll = HillTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hillTroll :: EnemyCard HillTroll
hillTroll = enemy HillTroll Cards.hillTroll

-- | Excess combat damage dealt by Hill Troll (damage that is dealt beyond the remaining hit
-- points of the character damaged by its attack) must be assigned as an increase to your
-- threat.
instance HasModifiersFor HillTroll where
  getModifiersFor (HillTroll a) = modified a a [ExcessDamageToThreat]
