module LordOfTheRings.Enemy.Cards.WolfOfAngmar (wolfOfAngmar) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)

newtype WolfOfAngmar = WolfOfAngmar EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfOfAngmar :: EnemyCard WolfOfAngmar
wolfOfAngmar = enemy WolfOfAngmar Cards.wolfOfAngmar

-- | Surge. While it is Night, Wolf of Angmar gets +2 attack.
instance HasModifiersFor WolfOfAngmar where
  getModifiersFor (WolfOfAngmar a) = whenM isNight $ modified a a [AttackModifier 2]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if it is Night).
instance RunMessage WolfOfAngmar where
  runMessage msg e@(WolfOfAngmar attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid . bool 1 2 =<< isNight
      pure e
    _ -> WolfOfAngmar <$> runMessage msg attrs
