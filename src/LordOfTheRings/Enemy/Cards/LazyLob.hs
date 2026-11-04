module LordOfTheRings.Enemy.Cards.LazyLob (lazyLob) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn)

newtype LazyLob = LazyLob EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Venom, on its card def.
lazyLob :: EnemyCard LazyLob
lazyLob = enemy LazyLob Cards.lazyLob

-- | When Revealed: Each character committed to the quest gets 1 poison.
-- Shadow: If defending character is poisoned, make that character unconscious.
instance RunMessage LazyLob where
  runMessage msg e@(LazyLob attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      for_ committed \c -> givePoison c 1
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> do
        poison <- poisonOn d
        when (poison > 0) $ makeUnconscious d
      pure e
    _ -> LazyLob <$> runMessage msg attrs
