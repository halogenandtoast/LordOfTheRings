module LordOfTheRings.Enemy.Cards.MountainGoblin (mountainGoblin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Mountain))

newtype MountainGoblin = MountainGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainGoblin :: EnemyCard MountainGoblin
mountainGoblin = enemy MountainGoblin Cards.mountainGoblin

-- | Mountain Goblin gets +1 [Attack] for each Mountain location in the staging area.
instance HasModifiersFor MountainGoblin where
  getModifiersFor (MountainGoblin a) = do
    mountains <- length <$> select (LocationInStagingArea <> LocationWithTrait Mountain)
    when (mountains > 0) $ modified a a [AttackModifier mountains]

-- | Shadow: attacking enemy gets +1 [Attack]. (+2 [Attack] instead if the active location
-- is a Mountain.)
instance RunMessage MountainGoblin where
  runMessage msg e@(MountainGoblin attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      mountain <- selectAny (IsActiveLocation <> LocationWithTrait Mountain)
      enemyGetsAttack attrs eid (if mountain then 2 else 1)
      pure e
    _ -> MountainGoblin <$> runMessage msg attrs
