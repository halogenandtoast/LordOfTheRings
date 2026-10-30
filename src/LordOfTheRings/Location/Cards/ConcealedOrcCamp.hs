module LordOfTheRings.Location.Cards.ConcealedOrcCamp (concealedOrcCamp) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (getMissionObjective)
import LordOfTheRings.Trait (Trait (Orc))

newtype ConcealedOrcCamp = ConcealedOrcCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

concealedOrcCamp :: LocationCard ConcealedOrcCamp
concealedOrcCamp = location ConcealedOrcCamp Cards.concealedOrcCamp

-- | While Concealed Orc-camp is the active location, it gains: "Response: When Concealed
-- Orc-camp is explored, place 1 resource token on the Mission objective."
instance HasAbilities ConcealedOrcCamp where
  getAbilities (ConcealedOrcCamp a) = [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

-- | Travel: The first player searches the encounter deck and discard pile for an Orc enemy
-- and puts it into play engaged with him. Shuffle the encounter deck.
instance RunMessage ConcealedOrcCamp where
  runMessage msg l@(ConcealedOrcCamp a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToEngage lead a (CardWithType EnemyType <> CardWithTrait Orc)
      ConcealedOrcCamp <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ (\m -> placeResources a m 1) =<< getMissionObjective
      pure l
    _ -> ConcealedOrcCamp <$> runMessage msg a
