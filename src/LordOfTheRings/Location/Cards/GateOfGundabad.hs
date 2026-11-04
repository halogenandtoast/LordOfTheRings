module LordOfTheRings.Location.Cards.GateOfGundabad (gateOfGundabad) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GateOfGundabad = GateOfGundabad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gateOfGundabad :: LocationCard GateOfGundabad
gateOfGundabad = location GateOfGundabad Cards.gateOfGundabad

-- | Immune to player card effects. While Gate of Gundabad is in the staging area, resources
-- cannot be placed on The Heroes' Defense (which reads it).
instance HasModifiersFor GateOfGundabad where
  getModifiersFor (GateOfGundabad a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Travel: Search the encounter deck, discard pile, and victory display for a Goblin Troop
-- and add it to the staging area. Shuffle the encounter deck.
instance RunMessage GateOfGundabad where
  runMessage msg (GateOfGundabad a) = case msg of
    TravelTo lid | lid == a.id -> do
      searchEncounterCardsAndVictoryDisplay (cardIs Enemies.goblinTroop) addToStaging
      GateOfGundabad <$> runMessage msg a
    _ -> GateOfGundabad <$> runMessage msg a
