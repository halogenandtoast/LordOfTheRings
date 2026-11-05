module LordOfTheRings.Treachery.Cards.FlamingSword (flamingSword) where

import LordOfTheRings.Keyword (Keyword (Regenerate))
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Weapons
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FlamingSword = FlamingSword TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

flamingSword :: TreacheryCard FlamingSword
flamingSword = treachery FlamingSword Cards.flamingSword

instance HasModifiersFor FlamingSword where
  getModifiersFor (FlamingSword a) = balrogWeaponModifiers a [AttackModifier 1, GainKeyword (Regenerate 4)]

instance RunMessage FlamingSword where
  runMessage msg (FlamingSword a) = FlamingSword <$> runBalrogWeapon msg a
