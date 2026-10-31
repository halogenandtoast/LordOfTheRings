module LordOfTheRings.Location.Cards.DustyLabyrinth (dustyLabyrinth) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (isScour, placeUnderOrcsSearch)

newtype DustyLabyrinth = DustyLabyrinth LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dustyLabyrinth :: LocationCard DustyLabyrinth
dustyLabyrinth = location DustyLabyrinth Cards.dustyLabyrinth

-- | Dusty Labyrinth gets +1 threat for each damage on it.
instance HasModifiersFor DustyLabyrinth where
  getModifiersFor (DustyLabyrinth a) = when (a.damage > 0) $ modifySelf a [ThreatModifier a.damage]

-- | Scour: Deal 2 damage to Dusty Labyrinth. From the discard pile it is not there to take
-- it.
-- Travel: The first player must place a card from his hand facedown underneath The Orcs'
-- Search to travel here.
instance RunMessage DustyLabyrinth where
  runMessage msg l@(DustyLabyrinth a) = case msg of
    _ | isScour a msg -> do
      unless (a.placement == Unplaced) $ placeDamage a a 2
      pure l
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      hand <- (.hand) <$> getPlayer lead
      chooseOrRunOneM lead do
        questionLabeled "dustyLabyrinth.place"
        causedBy a
        targets hand placeUnderOrcsSearch
      pure l
    _ -> DustyLabyrinth <$> runMessage msg a
