module LordOfTheRings.Enemy.Cards.GoblintownScavengers (goblintownScavengers) where

import LordOfTheRings.Card (CardCost (StaticCost), CardDef (..), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblintownScavengers = GoblintownScavengers EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblintownScavengers :: EnemyCard GoblintownScavengers
goblintownScavengers = enemy GoblintownScavengers Cards.goblintownScavengers

-- | When Revealed: Discard the top card of each player's deck. Until the end of the phase,
-- increase Goblintown Scavenger's threat by the total printed cost of all cards discarded
-- in this way.
instance RunMessage GoblintownScavengers where
  runMessage msg e@(GoblintownScavengers attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      let printed c = case cdCost (toCardDef c) of
            Just (StaticCost n) -> n
            _ -> 0
          total = sum [printed c | p <- players, c <- take 1 p.deck]
      eachPlayer \p -> discardTopOfDeck p 1
      when (total > 0) $ phaseModifier attrs attrs (ThreatModifier total)
      pure e
    _ -> GoblintownScavengers <$> runMessage msg attrs
