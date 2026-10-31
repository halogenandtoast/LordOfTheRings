module LordOfTheRings.Location.Cards.NarelenyaLocation (narelenyaLocation) where

import LordOfTheRings.Card (CardType (AllyType), cdCardType, lookupCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Raid on the Grey Havens Nightmare's Nárelenya, a location: not the fleet's Ship
-- objective.
newtype NarelenyaLocation = NarelenyaLocation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

narelenyaLocation :: LocationCard NarelenyaLocation
narelenyaLocation = location NarelenyaLocation Cards.narelenyaLocation

-- | Immune to player card effects.
instance HasModifiersFor NarelenyaLocation where
  getModifiersFor (NarelenyaLocation a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After the first player plays an ally from his hand, deal 1 damage to the
-- Nárelenya.
instance RunMessage NarelenyaLocation where
  runMessage msg (NarelenyaLocation a) = case msg of
    CheckWindows ws -> do
      lead <- getLead
      let allies = [() | Window After (PlayedCardFromHand pid code) <- ws, pid == lead, (cdCardType <$> lookupCardDef code) == Just AllyType]
      for_ allies \_ -> dealDamage a a.id 1
      NarelenyaLocation <$> runMessage msg a
    _ -> NarelenyaLocation <$> runMessage msg a
