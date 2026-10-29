module LordOfTheRings.Enemy.Cards.BellachTroubleInTharbad (bellachTroubleInTharbad) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Creature, Orc))

newtype BellachTroubleInTharbad = BellachTroubleInTharbad EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

bellachTroubleInTharbad :: EnemyCard BellachTroubleInTharbad
bellachTroubleInTharbad = enemy BellachTroubleInTharbad Cards.bellachTroubleInTharbad

-- | X is the number of players in the game.
-- Orc and Creature enemies get -30 engagement cost and +1 threat.
-- Forced: When Bellach is destroyed, shuffle him into the encounter deck.
instance HasModifiersFor BellachTroubleInTharbad where
  getModifiersFor (BellachTroubleInTharbad a) = do
    players <- length <$> getRemainingPlayers
    modified a a [ThreatModifier players, ShuffledIntoEncounterDeckWhenDefeated]
    followers <- (<>) <$> select (EnemyWithTrait Orc) <*> select (EnemyWithTrait Creature)
    for_ (ordNub followers) \e -> modified a e [EngagementCostModifier (-30), ThreatModifier 1]

instance RunMessage BellachTroubleInTharbad where
  runMessage msg (BellachTroubleInTharbad attrs) = BellachTroubleInTharbad <$> runMessage msg attrs
