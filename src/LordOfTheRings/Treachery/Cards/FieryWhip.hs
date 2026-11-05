module LordOfTheRings.Treachery.Cards.FieryWhip (fieryWhip) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Weapons
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FieryWhip = FieryWhip TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fieryWhip :: TreacheryCard FieryWhip
fieryWhip = treachery FieryWhip Cards.fieryWhip

instance HasModifiersFor FieryWhip where
  getModifiersFor (FieryWhip a) = balrogWeaponModifiers a [DefenseModifier 1, GainKeyword (Archery 4)]

instance RunMessage FieryWhip where
  runMessage msg (FieryWhip a) = FieryWhip <$> runBalrogWeapon msg a
