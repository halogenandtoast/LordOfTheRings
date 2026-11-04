module LordOfTheRings.Enemy.Cards.WildMumak (wildMumak) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WildMumak = WildMumak EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Indestructible, on its card def: it leaves play as a Capture objective adds it to the
-- victory display.
wildMumak :: EnemyCard WildMumak
wildMumak = enemy WildMumak Cards.wildMumak

-- | Immune to player card effects. Cannot take more than 3 damage each round.
instance HasModifiersFor WildMumak where
  getModifiersFor (WildMumak a) = modifySelf a [ImmuneToPlayerCardEffects, DamageLimit 3]
