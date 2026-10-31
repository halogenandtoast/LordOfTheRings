module LordOfTheRings.Enemy.Cards.MumakElite (mumakElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Archery))

newtype MumakElite = MumakElite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mumakElite :: EnemyCard MumakElite
mumakElite = enemy MumakElite Cards.mumakElite

-- | Cannot have attachments. Archery X.
-- X is the number of players in the game.
-- Mumak Elite cannot take more than 3 damage each round.
instance HasModifiersFor MumakElite where
  getModifiersFor (MumakElite a) = do
    players <- length <$> getRemainingPlayers
    modified a a [CannotHaveAttachments, DamageLimit 3, GainKeyword (Archery players)]

-- | Damage past the round's limit is lost, as Mumak's.
instance RunMessage MumakElite where
  runMessage msg e@(MumakElite attrs) = case msg of
    PlaceTokens source (isTarget attrs -> True) Damage n | n > 0 -> do
      left <- foldr min maxBound . (\ms -> [k | DamageLimit k <- ms]) <$> getModifiers attrs
      let placed = min n left
      roundModifier attrs attrs (DamageLimit (left - placed))
      if placed > 0
        then MumakElite <$> runMessage (PlaceTokens source (toTarget attrs) Damage placed) attrs
        else pure e
    _ -> MumakElite <$> runMessage msg attrs
