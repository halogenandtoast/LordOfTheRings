module LordOfTheRings.Location.Cards.DenseJungle (denseJungle) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DenseJungle = DenseJungle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

denseJungle :: LocationCard DenseJungle
denseJungle = location DenseJungle Cards.denseJungle

-- | Forced: When Dense Jungle is explored, discard the top card of the encounter deck. If
-- that card is a location, add that location to the staging area.
instance HasAbilities DenseJungle where
  getAbilities (DenseJungle a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Shadow: Discard a random card from your hand.
instance RunMessage DenseJungle where
  runMessage msg l@(DenseJungle a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardTopOfEncounterDeck a 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType LocationType) cards) addToStagingFromDiscard
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardRandomFromHand pid 1
      pure l
    _ -> DenseJungle <$> runMessage msg a
