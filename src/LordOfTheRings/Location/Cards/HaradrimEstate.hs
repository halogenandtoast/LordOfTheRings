module LordOfTheRings.Location.Cards.HaradrimEstate (haradrimEstate) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Harad))

newtype HaradrimEstate = HaradrimEstate LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimEstate :: LocationCard HaradrimEstate
haradrimEstate = location HaradrimEstate Cards.haradrimEstate

-- | While Haradrim Estate is in the staging area, progress cannot be placed here.
instance HasModifiersFor HaradrimEstate where
  getModifiersFor (HaradrimEstate a) = when (a.placement == StagingArea) $ modifySelf a [ProgressLimit 0]

-- | Travel: Search the encounter deck and discard pile for a Harad enemy and put it into
-- play engaged with the first player. Then, place 1 progress on the main quest, bypassing
-- the active location. Shuffle the encounter deck.
instance RunMessage HaradrimEstate where
  runMessage msg (HaradrimEstate a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToEngage lead a (CardWithType EnemyType <> CardWithTrait Harad)
      placeProgressOnMainQuest a 1
      HaradrimEstate <$> runMessage msg a
    _ -> HaradrimEstate <$> runMessage msg a
