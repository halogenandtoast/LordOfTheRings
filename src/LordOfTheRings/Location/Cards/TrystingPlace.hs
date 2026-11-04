module LordOfTheRings.Location.Cards.TrystingPlace (trystingPlace) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (oathbreakerEnemy)

newtype TrystingPlace = TrystingPlace LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

trystingPlace :: LocationCard TrystingPlace
trystingPlace = location TrystingPlace Cards.trystingPlace

-- | While Trysting Place is in the staging area, each enemy in the staging area cannot take
-- damage.
instance HasModifiersFor TrystingPlace where
  getModifiersFor (TrystingPlace a) = when (a.placement == StagingArea) do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [CannotTakeDamage]

-- | Travel: The first player searches the encounter deck and discard pile for an Oathbreaker
-- enemy and puts it into play engaged with him. Shuffle the encounter deck.
instance RunMessage TrystingPlace where
  runMessage msg (TrystingPlace a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToEngage lead a oathbreakerEnemy
      TrystingPlace <$> runMessage msg a
    _ -> TrystingPlace <$> runMessage msg a
