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
      pushAll [CreateEffect $ Effect (toSource attrs) c [WillpowerModifier (-1)] UntilEndOfPhase | c <- committed]
      pure e
    ResolveShadowEffect cid _ pid defender | cid == attrs.cardId -> do
      push $ RaiseThreat pid (if isJust defender then 4 else 8)
      pure e
    _ -> UngoliantsSpawn <$> runMessage msg attrs
