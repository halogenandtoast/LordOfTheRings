module LordOfTheRings.Location.Cards.SarnGebirRapids (sarnGebirRapids) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Hazard))

newtype SarnGebirRapids = SarnGebirRapids LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

sarnGebirRapids :: LocationCard SarnGebirRapids
sarnGebirRapids = location SarnGebirRapids Cards.sarnGebirRapids

-- | While Sarn Gebir Rapids is the active location, the "when revealed" effects of Hazard
-- cards cannot be canceled.
instance HasModifiersFor SarnGebirRapids where
  getModifiersFor (SarnGebirRapids a) = when (a.placement == ActiveLocation) do
    hazards <- getsGame (filter ((Hazard `member`) . cdCardTraits . toCardDef) . toList . (.entities.treacheries))
    for_ hazards \t -> modified a (toId t) [WhenRevealedCannotBeCanceled]
