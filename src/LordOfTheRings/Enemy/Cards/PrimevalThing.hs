module LordOfTheRings.Enemy.Cards.PrimevalThing (primevalThing) where

import Data.List qualified as L
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (attachFromTopOfDeck, namelessX)

newtype PrimevalThing = PrimevalThing EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

primevalThing :: EnemyCard PrimevalThing
primevalThing = enemy PrimevalThing Cards.primevalThing

-- | X is the printed cost of all attached cards on this card. If there are no cards
-- attached, X is 6.
instance HasModifiersFor PrimevalThing where
  getModifiersFor (PrimevalThing a) = let x = namelessX 6 a in modified a a [AttackModifier x, HitPointModifier x]

-- | Forced: When Primeval Thing enters play, attach cards from the top of the first
-- player's deck to it until the total printed cost of all attached cards is 6 or higher.
instance RunMessage PrimevalThing where
  runMessage msg (PrimevalThing attrs) = case msg of
    -- entering play, to the staging area or engaged; one that comes back to the staging
    -- area already holds its cards
    CheckWindows ws | null attrs.facedown && any (`elem` ws) [Window After (EntersPlay self), Window After (EntersStagingArea self)] -> do
      lead <- getLead
      deck <- (.deck) <$> getPlayer lead
      let costs = [printedCostOf pc | PlayerCard pc <- deck]
          -- the cards up to and with the one that brings the total to 6
          n = length (takeWhile (< 6) (L.scanl (+) 0 costs))
      attachFromTopOfDeck lead (min n (length costs)) attrs
      PrimevalThing <$> runMessage msg attrs
     where
      self = toTarget attrs
    _ -> PrimevalThing <$> runMessage msg attrs
