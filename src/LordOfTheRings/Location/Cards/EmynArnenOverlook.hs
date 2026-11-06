module LordOfTheRings.Location.Cards.EmynArnenOverlook (emynArnenOverlook) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge, Doomed))
import LordOfTheRings.Trait (Trait (Scout))

newtype EmynArnenOverlook = EmynArnenOverlook LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

emynArnenOverlook :: LocationCard EmynArnenOverlook
emynArnenOverlook = location EmynArnenOverlook Cards.emynArnenOverlook

instance HasModifiersFor EmynArnenOverlook where
  getModifiersFor (EmynArnenOverlook a) = modified a GameTarget [FirstRevealedEnemyWithTraitGainsKeywords Scout [Surge, Doomed 2]]
