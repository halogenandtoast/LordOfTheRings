module LordOfTheRings.Enemy.Cards.ShadyBreeLander (shadyBreeLander) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ShadyBreeLander = ShadyBreeLander EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadyBreeLander :: EnemyCard ShadyBreeLander
shadyBreeLander = enemy ShadyBreeLander Cards.shadyBreeLander

-- | While The One Ring is exhausted, Shady Bree-lander gains: "Cannot be optionally
-- engaged."
instance HasModifiersFor ShadyBreeLander where
  getModifiersFor (ShadyBreeLander a) = do
    exhausted <- theOneRingExhausted
    when exhausted $ modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Shadow: Defending character gets -1 defense (-2 defense instead if The One Ring is
-- exhausted).
instance RunMessage ShadyBreeLander where
  runMessage msg e@(ShadyBreeLander attrs) = case msg of
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      exhausted <- theOneRingExhausted
      for_ defenders \d -> attackModifier attrs d (DefenseModifier $ if exhausted then -2 else -1)
      pure e
    _ -> ShadyBreeLander <$> runMessage msg attrs
