module LordOfTheRings.Ally.Cards.Guthlaf (guthlaf) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Trait (Trait (Gondor, Rohan))

newtype Guthlaf = Guthlaf AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

guthlaf :: AllyCard Guthlaf
guthlaf = ally Guthlaf Cards.guthlaf

-- | If there is at least 1 Rohan hero in play, lower the cost to play Guthlaf by 1. If
-- there is at least 1 Gondor hero in play, Guthlaf gains sentinel.
instance HasModifiersFor Guthlaf where
  getModifiersFor (Guthlaf a)
    | a.placement == Unplaced = do
        rohan <- selectAny (HeroWithTrait Rohan)
        when rohan $ modified a (CardIdTarget a.allyCardId) [CostModifier (-1)]
    | otherwise = do
        gondor <- selectAny (HeroWithTrait Gondor)
        when gondor $ modified a a [GainKeyword Sentinel]
