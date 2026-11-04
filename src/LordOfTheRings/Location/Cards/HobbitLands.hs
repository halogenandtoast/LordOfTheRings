module LordOfTheRings.Location.Cards.HobbitLands (hobbitLands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HobbitLands = HobbitLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hobbitLands :: LocationCard HobbitLands
hobbitLands = location HobbitLands Cards.hobbitLands

-- | Response: After placing 1 or more progress tokens on Hobbit-lands, the first player
-- draws 1 card.
instance HasAbilities HobbitLands where
  getAbilities (HobbitLands a) = [mkAbility a 1 $ response $ ProgressPlacedWindow After (toTarget a)]

-- | Shadow: Defending player must put the top card of the encounter deck discard pile on
-- top of the encounter deck.
instance RunMessage HobbitLands where
  runMessage msg l@(HobbitLands a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      drawCards a lead 1
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      traverse_ moveToTopOfEncounterDeck . headMay =<< getEncounterDiscard
      pure l
    _ -> HobbitLands <$> runMessage msg a
