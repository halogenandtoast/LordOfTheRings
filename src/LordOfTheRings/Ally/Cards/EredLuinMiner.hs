module LordOfTheRings.Ally.Cards.EredLuinMiner (eredLuinMiner) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype EredLuinMiner = EredLuinMiner AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Response: After Ered Luin Miner is discarded from your deck, put it into play under
-- your control.
eredLuinMiner :: AllyCard EredLuinMiner
eredLuinMiner =
  ally
    (\a -> EredLuinMiner a {allyDiscardWindow = Just $ DiscardedFromDeckWindow After a.owner (Just a.allyCardId)})
    Cards.eredLuinMiner
