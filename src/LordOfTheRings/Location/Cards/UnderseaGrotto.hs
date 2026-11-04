module LordOfTheRings.Location.Cards.UnderseaGrotto (underseaGrotto) where

import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Grotto side; its Underwater side is Dark Abyss.
newtype UnderseaGrotto = UnderseaGrotto LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underseaGrotto :: LocationCard UnderseaGrotto
underseaGrotto = location UnderseaGrotto Cards.underseaGrotto

-- | While Undersea Grotto is the active location, reduce the cost of the first ally played
-- by the players each round by 1: a discount for the round as it becomes active, and again
-- as each round begins while it is.
-- Response: After you travel to Undersea Grotto, you may flip it to its Underwater side.
instance HasAbilities UnderseaGrotto where
  getAbilities (UnderseaGrotto a) =
    [ mkAbility a 1 $ silent $ LocationBecameActiveWindow After (LocationWithId a.id)
    , mkAbility a 3 $ response $ TraveledToWindow After (LocationWithId a.id)
    ]
      <> [mkAbility a 2 $ silent $ PhaseBeginsWindow After ResourcePhase | a.placement == ActiveLocation]

instance RunMessage UnderseaGrotto where
  runMessage msg l@(UnderseaGrotto a) = case msg of
    UseCardAbility _ (isSource a -> True) n _ | n `elem` [1, 2] -> do
      roundModifier a GameTarget (ReduceNextCostOf (CardWithType AllyType) 1)
      pure l
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      flipOver a
      pure l
    _ -> UnderseaGrotto <$> runMessage msg a
