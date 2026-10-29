module LordOfTheRings.Location.Cards.HousesOfTheDead (housesOfTheDead) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype HousesOfTheDead = HousesOfTheDead LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

housesOfTheDead :: LocationCard HousesOfTheDead
housesOfTheDead = location HousesOfTheDead Cards.housesOfTheDead

-- | Underworld 2.
-- Forced: After Houses of the Dead becomes the active location, exhaust all characters.
-- Read off it becoming active however it does: traveled to, made active by a card (as
-- Strider's Path), or put into play active, once it is.

instance RunMessage HousesOfTheDead where
  runMessage msg l@(HousesOfTheDead a) = case msg of
    PlaceLocation lid ActiveLocation | lid == a.id && a.placement /= ActiveLocation -> do
      doStep 1 msg
      HousesOfTheDead <$> runMessage msg a
    DoStep 1 (PlaceLocation lid ActiveLocation) | lid == a.id -> exhaustAll
    DoStep 1 (PutIntoPlayAsActiveLocation card) | card.id == a.cardId -> exhaustAll
    _ -> HousesOfTheDead <$> runMessage msg a
   where
    exhaustAll = do
      traverse_ exhaust =<< select ReadyHero
      traverse_ exhaust =<< select ReadyAlly
      pure l
