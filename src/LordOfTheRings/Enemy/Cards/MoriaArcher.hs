module LordOfTheRings.Enemy.Cards.MoriaArcher (moriaArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MoriaArcher = MoriaArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

moriaArcher :: EnemyCard MoriaArcher
moriaArcher = enemy MoriaArcher Cards.moriaArcher

-- | Peril. Archery 2. X is the number of players in the game (its threat).
instance HasModifiersFor MoriaArcher where
  getModifiersFor (MoriaArcher a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ThreatModifier players]

-- | When Revealed: Assign X damage among characters you control.
instance RunMessage MoriaArcher where
  runMessage msg e@(MoriaArcher attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      players <- length <$> getRemainingPlayers
      dealDamageDivided attrs you players
      pure e
    _ -> MoriaArcher <$> runMessage msg attrs
