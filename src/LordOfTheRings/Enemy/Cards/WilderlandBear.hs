module LordOfTheRings.Enemy.Cards.WilderlandBear (wilderlandBear) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WilderlandBear = WilderlandBear EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wilderlandBear :: EnemyCard WilderlandBear
wilderlandBear = enemy WilderlandBear Cards.wilderlandBear

-- | When Revealed: The first player must choose: either Wilderland Bear makes an immediate
-- attack against him, or each player discards 1 random card from his hand.
instance RunMessage WilderlandBear where
  runMessage msg e@(WilderlandBear attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "wilderlandBear.choose"
        causedBy attrs
        labeled "wilderlandBear.attack" $ immediateAttack lead attrs.id
        labeled "wilderlandBear.discard" $ eachPlayer \p -> discardRandomFromHand p 1
      pure e
    _ -> WilderlandBear <$> runMessage msg attrs
