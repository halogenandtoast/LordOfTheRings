module LordOfTheRings.Location.Cards.IronHillsMine (ironHillsMine) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype IronHillsMine = IronHillsMine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ironHillsMine :: LocationCard IronHillsMine
ironHillsMine = location IronHillsMine Cards.ironHillsMine

-- | While Iron Hills Mine is the active location, it gains: "Response: When Iron Hills
-- Mine is explored, each player may return 1 card from his discard pile to his hand."
instance HasAbilities IronHillsMine where
  getAbilities (IronHillsMine a) = [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

-- | Travel: Each player discards the top 4 cards of his deck to travel here.
instance RunMessage IronHillsMine where
  runMessage msg l@(IronHillsMine a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (`discardTopOfDeck` 4)
      IronHillsMine <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer $ mayReturnFromDiscardToHand a
      pure l
    _ -> IronHillsMine <$> runMessage msg a
