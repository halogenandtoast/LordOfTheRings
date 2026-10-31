module LordOfTheRings.Enemy.Cards.VenomsBite (venomsBite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | Boarding 2. Its *Forced, as a Sailing test discards it, is Flight of the Stormcaller's.
newtype VenomsBite = VenomsBite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

venomsBite :: EnemyCard VenomsBite
venomsBite = enemy VenomsBite Cards.venomsBite

-- | Cannot have attachments.
instance HasModifiersFor VenomsBite where
  getModifiersFor (VenomsBite a) = modifySelf a [CannotHaveAttachments]

instance RunMessage VenomsBite where
  runMessage msg (VenomsBite attrs) = VenomsBite <$> runMessage msg attrs
