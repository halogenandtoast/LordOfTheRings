module LordOfTheRings.Enemy.Cards.Mumak (mumak) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype Mumak = Mumak EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mumak :: EnemyCard Mumak
mumak = enemy Mumak Cards.mumak

-- | No attachments can be attached to Mumak.
-- Mumak cannot take more than 3 damage each round.
instance HasModifiersFor Mumak where
  getModifiersFor (Mumak a) = modified a a [CannotHaveAttachments, DamageLimit 3]

-- | Damage past the round's limit is lost; what the round has left is kept as a round
-- effect, as Perilous Swamp keeps its progress.
instance RunMessage Mumak where
  runMessage msg e@(Mumak attrs) = case msg of
    PlaceTokens source (isTarget attrs -> True) Damage n | n > 0 -> do
      left <- foldr min maxBound . (\ms -> [k | DamageLimit k <- ms]) <$> getModifiers attrs
      let placed = min n left
      roundModifier attrs attrs (DamageLimit (left - placed))
      if placed > 0
        then Mumak <$> runMessage (PlaceTokens source (toTarget attrs) Damage placed) attrs
        else pure e
    _ -> Mumak <$> runMessage msg attrs
