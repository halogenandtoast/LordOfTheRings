module LordOfTheRings.Enemy.Cards.MumakElite (mumakElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))

newtype MumakElite = MumakElite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
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
