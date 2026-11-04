module LordOfTheRings.Contract.Cards.BondOfFriendship (bondOfFriendship) where

import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted

newtype BondOfFriendship = BondOfFriendship ContractAttrs
  deriving anyclass (IsContract, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Your deck must be exactly 50 player cards and include exactly 10 cards from each of
these four spheres: Leadership, Lore, Spirit, and Tactics. You cannot include more than 2
copies of any card by title in your deck. When choosing your starting heroes, you may
choose 4 heroes instead of 3. Each of your heroes must belong to a different sphere.

All of it is deckbuilding, checked as the deck is chosen ('LordOfTheRings.Decklist').
-}
bondOfFriendship :: ContractCard BondOfFriendship
bondOfFriendship = contract BondOfFriendship Cards.bondOfFriendship

instance RunMessage BondOfFriendship where
  runMessage msg (BondOfFriendship attrs) = BondOfFriendship <$> runMessage msg attrs
