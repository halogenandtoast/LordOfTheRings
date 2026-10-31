module LordOfTheRings.Location.Cards.AnduinWharf (anduinWharf) where

import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (runNightmareBattleground)

newtype AnduinWharf = AnduinWharf LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

anduinWharf :: LocationCard AnduinWharf
anduinWharf = location AnduinWharf Cards.anduinWharf

-- | While Anduin Wharf is the active location, allies cost 1 additional matching resource
-- to play from hand.
instance HasModifiersFor AnduinWharf where
  getModifiersFor (AnduinWharf a) =
    when (a.placement == ActiveLocation) $ modified a GameTarget [IncreaseCost (CardWithType AllyType) 1]

-- | If Anduin Wharf has 6 or more damage, discard it and reveal 2 cards from the top of the
-- encounter deck.
instance RunMessage AnduinWharf where
  runMessage msg (AnduinWharf a) = AnduinWharf <$> runNightmareBattleground (revealEncounterCards 2) msg a
