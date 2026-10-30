module LordOfTheRings.Location.Cards.WhiteShip (whiteShip) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Corsair, Raider))

newtype WhiteShip = WhiteShip LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

whiteShip :: LocationCard WhiteShip
whiteShip = location WhiteShip Cards.whiteShip

-- | Travel: Search the encounter deck and discard pile for a Corsair enemy and add it to
-- the staging area. Shuffle the encounter deck.
-- Forced: When White Ship is placed underneath The Havens Burn, place 1 resource on each
-- Raider enemy in play.
instance RunMessage WhiteShip where
  runMessage msg l@(WhiteShip a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead a (CardWithType EnemyType <> CardWithTrait Corsair)
      pure l
    PlaceUnderneath _ cards | any ((== a.cardId) . (.id)) cards -> do
      raiders <- select (EnemyWithTrait Raider)
      for_ raiders \r -> placeResources a r 1
      WhiteShip <$> runMessage msg a
    _ -> WhiteShip <$> runMessage msg a
