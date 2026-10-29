module LordOfTheRings.Enemy.Cards.DunlandProwler (dunlandProwler) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))

newtype DunlandProwler = DunlandProwler EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandProwler :: EnemyCard DunlandProwler
dunlandProwler = enemy DunlandProwler Cards.dunlandProwler

-- | While any player has 3 or more cards in hand, Dunland Prowler gains Surge. While any
-- player has 5 or more cards in hand, Dunland Prowler gets +1 threat.
instance HasModifiersFor DunlandProwler where
  getModifiersFor (DunlandProwler a) = do
    most <- foldr max 0 <$> (traverse (fmap (length . (.hand)) . getPlayer) =<< getRemainingPlayers)
    modified a a $ [GainKeyword Surge | most >= 3] <> [ThreatModifier 1 | most >= 5]

instance RunMessage DunlandProwler where
  runMessage msg (DunlandProwler attrs) = DunlandProwler <$> runMessage msg attrs
