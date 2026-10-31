module LordOfTheRings.Enemy.Cards.UngoliantsSpawnNightmare (ungoliantsSpawnNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UngoliantsSpawnNightmare = UngoliantsSpawnNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ungoliantsSpawnNightmare :: EnemyCard UngoliantsSpawnNightmare
ungoliantsSpawnNightmare = enemy UngoliantsSpawnNightmare Cards.ungoliantsSpawnNightmare

-- | When Revealed: Until the end of the phase, each character currently committed to the
-- quest gets -2 willpower and is discarded if its willpower is 0.
-- Shadow: Raise the defending player's threat by 5. (Raise the defending player's threat by
-- 10 instead if undefended.)
instance RunMessage UngoliantsSpawnNightmare where
  runMessage msg e@(UngoliantsSpawnNightmare attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      for_ committed \c -> phaseModifiers attrs c [WillpowerModifier (-2), DiscardedAtNoWillpower]
      pure e
    ResolveShadowEffect cid _ pid defender | cid == attrs.cardId -> do
      encounterRaisesThreat pid (ifUndefended defender 10 5)
      pure e
    _ -> UngoliantsSpawnNightmare <$> runMessage msg attrs
