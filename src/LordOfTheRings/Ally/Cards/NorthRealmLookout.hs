module LordOfTheRings.Ally.Cards.NorthRealmLookout (northRealmLookout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Item))

newtype NorthRealmLookout = NorthRealmLookout AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

northRealmLookout :: AllyCard NorthRealmLookout
northRealmLookout = ally NorthRealmLookout Cards.northRealmLookout

-- | North Realm Lookout gets +1 willpower and does not exhaust to quest while it has an
-- Item attachment.
instance HasModifiersFor NorthRealmLookout where
  getModifiersFor (NorthRealmLookout a) = do
    equipped <- notNull <$> attachedWithTrait Item (toTarget a)
    when equipped $ modifySelf a [WillpowerModifier 1, DoesNotExhaustToCommit]
