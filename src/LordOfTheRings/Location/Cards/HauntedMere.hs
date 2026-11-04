module LordOfTheRings.Location.Cards.HauntedMere (hauntedMere) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (exhaustSmeagolToTravel, searchDeckAndDiscardFor, smeagolTravelModifiers, undeadEnemy)

newtype HauntedMere = HauntedMere LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntedMere :: LocationCard HauntedMere
hauntedMere = location HauntedMere Cards.hauntedMere

-- | Travel: Exhaust Sméagol to travel here.
instance HasModifiersFor HauntedMere where
  getModifiersFor (HauntedMere a) = smeagolTravelModifiers a

-- | Mire 2.
-- Forced: When Haunted Mere is discarded by the Mire keyword, search the encounter deck and
-- discard pile for an Undead enemy, reveal it, and add it to the staging area. Shuffle the
-- encounter deck.
instance HasAbilities HauntedMere where
  getAbilities (HauntedMere a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

-- | The first player searches.
instance RunMessage HauntedMere where
  runMessage msg l@(HauntedMere a) = case msg of
    TravelTo lid | lid == a.id -> do
      exhaustSmeagolToTravel
      HauntedMere <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      searchDeckAndDiscardFor lead "hauntedMere.search" undeadEnemy (push . RevealCard)
      pure l
    _ -> HauntedMere <$> runMessage msg a
