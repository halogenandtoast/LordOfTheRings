{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Contract (module LordOfTheRings.Contract, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Contract.Cards.BondOfFriendship
import LordOfTheRings.Contract.Cards.CouncilOfTheWise
import LordOfTheRings.Contract.Cards.Fellowship
import LordOfTheRings.Contract.Cards.ForthTheThreeHunters
import LordOfTheRings.Contract.Cards.MessengerOfTheKing
import LordOfTheRings.Contract.Cards.TheBurglarsTurn
import LordOfTheRings.Contract.Cards.TheGreyWanderer
import LordOfTheRings.Contract.Runner ()
import LordOfTheRings.Contract.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored contract is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Contract where
  parseJSON v = do
    attrs <- parseJSON @ContractAttrs v
    pure $ overAttrs (const attrs) $ lookupContract (contractCardCode attrs) (contractCardId attrs) (contractId attrs) (contractOwner attrs)

instance RunMessage Contract where
  runMessage msg (Contract a) = Contract <$> runMessage msg a

lookupContract :: (HasCallStack) => CardCode -> CardId -> ContractId -> PlayerId -> Contract
lookupContract cardCode cardId ident pid = case Map.lookup cardCode allContracts of
  Just (SomeContractCard builder) -> Contract $ cbCardBuilder builder cardId (ident, pid)
  Nothing -> error $ "Unknown contract: " <> show cardCode

-- | The player's contract, in play with his heroes: it is known by its card's id, so a
-- game can be dealt with it in play.
createContract :: PlayerId -> PlayerCard -> Contract
createContract pid card = lookupContract (toCardCode card) card.id (ContractId $ unCardId card.id) pid

allContracts :: Map CardCode SomeContractCard
allContracts =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeContractCard builder) <-
        [ SomeContractCard fellowship
        , SomeContractCard theBurglarsTurn
        , SomeContractCard forthTheThreeHunters
        , SomeContractCard theGreyWanderer
        , SomeContractCard councilOfTheWise
        , SomeContractCard messengerOfTheKing
        , SomeContractCard bondOfFriendship
        ]
    ]
