module LordOfTheRings.Contract.CardDefs (module LordOfTheRings.Contract.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.Prelude

allContractCards :: Map CardCode CardDef
allContractCards =
  Map.fromList
    $ map
      (\def -> (cdCardCode def, def))
      [fellowship, theBurglarsTurn, forthTheThreeHunters, theGreyWanderer, councilOfTheWise, messengerOfTheKing, bondOfFriendship]

fellowship :: CardDef
fellowship = (contract "21074" "Fellowship") {cdDoubleSided = True}

theBurglarsTurn :: CardDef
theBurglarsTurn = (contract "22024" "The Burglar's Turn") {cdDoubleSided = True}

forthTheThreeHunters :: CardDef
forthTheThreeHunters = (contract "22049" "Forth, The Three Hunters!") {cdDoubleSided = True}

theGreyWanderer :: CardDef
theGreyWanderer = (contract "22074" "The Grey Wanderer") {cdDoubleSided = True}

councilOfTheWise :: CardDef
councilOfTheWise = contract "22091" "Council of the Wise"

messengerOfTheKing :: CardDef
messengerOfTheKing = (contract "22134" "Messenger of the King") {cdDoubleSided = True}

bondOfFriendship :: CardDef
bondOfFriendship = contract "22147" "Bond of Friendship"
