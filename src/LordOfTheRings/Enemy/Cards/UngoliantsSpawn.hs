module LordOfTheRings.Enemy.Cards.UngoliantsSpawn (ungoliantsSpawn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UngoliantsSpawn = UngoliantsSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ungoliantsSpawn :: EnemyCard UngoliantsSpawn
ungoliantsSpawn = enemy UngoliantsSpawn Cards.ungoliantsSpawn

instance RunMessage UngoliantsSpawn where
  runMessage msg e@(UngoliantsSpawn attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      for_ committed \c -> phaseModifier attrs c (WillpowerModifier (-1))
      pure e
    ResolveShadowEffect cid _ pid defender | cid == attrs.cardId -> do
      raiseThreat pid (ifUndefended defender 8 4)
      pure e
    _ -> UngoliantsSpawn <$> runMessage msg attrs
