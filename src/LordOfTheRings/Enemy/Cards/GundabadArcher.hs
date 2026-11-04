module LordOfTheRings.Enemy.Cards.GundabadArcher (gundabadArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype GundabadArcher = GundabadArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gundabadArcher :: EnemyCard GundabadArcher
gundabadArcher = enemy GundabadArcher Cards.gundabadArcher

-- | When Revealed: Each player must deal 2 damage to a character he controls.
-- Shadow: Deal 1 damage to defending character for each quest stage in play with no
-- progress on it.
instance RunMessage GundabadArcher where
  runMessage msg e@(GundabadArcher attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> dealDamageToOneCharacter attrs pid 2
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      n <- stagesWithoutProgress
      when (n > 0) $ for_ defenders \d -> dealDamage (CardIdSource cid) d n
      pure e
    _ -> GundabadArcher <$> runMessage msg attrs
