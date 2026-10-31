module LordOfTheRings.Enemy.Cards.HaradrimWarrior (haradrimWarrior) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (controlledLocations)

newtype HaradrimWarrior = HaradrimWarrior EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimWarrior :: EnemyCard HaradrimWarrior
haradrimWarrior = enemy HaradrimWarrior Cards.haradrimWarrior

-- | X threat. Archery X. X is the number of locations controlled by the first player.
instance HasModifiersFor HaradrimWarrior where
  getModifiersFor (HaradrimWarrior a) = do
    x <- length <$> (controlledLocations =<< getLead)
    when (x > 0) $ modified a a [ThreatModifier x, GainKeyword (Archery x)]

-- | Forced: After Haradrim Warrior engages a player, it gets +X attack and +X defense until
-- the end of the round, X counted as it engages.
instance HasAbilities HaradrimWarrior where
  getAbilities (HaradrimWarrior a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Deal 1 damage to defending character.
instance RunMessage HaradrimWarrior where
  runMessage msg e@(HaradrimWarrior attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- length <$> (controlledLocations =<< getLead)
      when (x > 0) $ roundModifiers attrs attrs [AttackModifier x, DefenseModifier x]
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> HaradrimWarrior <$> runMessage msg attrs
