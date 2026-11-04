module LordOfTheRings.Location.Cards.UnderseaGrotto (underseaGrotto) where

import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Grotto side; its Underwater side is Dark Abyss.
newtype UnderseaGrotto = UnderseaGrotto LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

underseaGrotto :: LocationCard UnderseaGrotto
underseaGrotto = location UnderseaGrotto Cards.underseaGrotto

-- | While Undersea Grotto is the active location, reduce the cost of the first ally played
-- by the players each round by 1: until one has been played this round.
instance HasModifiersFor UnderseaGrotto where
  getModifiersFor (UnderseaGrotto a) = when (a.placement == ActiveLocation) do
    played <- any (\(_, _, t) -> t == AllyType) <$> cardsPlayedThisRound
    unless played do
      players <- getRemainingPlayers
      for_ players \p -> modified a (PlayerTarget p) [ReduceCostOf (CardWithType AllyType) 1]

-- | Response: After you travel to Undersea Grotto, you may flip it to its Underwater side.
instance HasAbilities UnderseaGrotto where
  getAbilities (UnderseaGrotto a) = [mkAbility a 1 $ response $ TraveledToWindow After (LocationWithId a.id)]

instance RunMessage UnderseaGrotto where
  runMessage msg l@(UnderseaGrotto a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    _ -> UnderseaGrotto <$> runMessage msg a
