module LordOfTheRings.Ally.Cards.CitadelCustodian (citadelCustodian) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Trait (Trait (Gondor))

newtype CitadelCustodian = CitadelCustodian AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

citadelCustodian :: AllyCard CitadelCustodian
citadelCustodian = ally CitadelCustodian Cards.citadelCustodian

-- | Lower the cost to play Citadel Custodian by 1 for each Gondor ally in play.
instance HasModifiersFor CitadelCustodian where
  getModifiersFor (CitadelCustodian a) = when (a.placement == Unplaced) do
    n <- length <$> select (AllyWithTrait Gondor)
    when (n > 0) $ modified a (CardIdTarget a.allyCardId) [CostModifier (negate n)]
