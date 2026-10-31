module LordOfTheRings.Enemy.Cards.UnderseaCaveTroll (underseaCaveTroll) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getDefenseAgainst, getHitPoints)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype UnderseaCaveTroll = UnderseaCaveTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underseaCaveTroll :: EnemyCard UnderseaCaveTroll
underseaCaveTroll = enemy UnderseaCaveTroll Cards.underseaCaveTroll

-- | The damage its attack deals the defender beyond its remaining hit points.
excessOn :: HasGame m => EnemyId -> Target -> m Int
excessOn eid defender = do
  attack <- getEnemyAttack eid
  defense <- getDefenseAgainst eid defender
  remaining <- (-) <$> getHitPoints defender <*> getDamage defender
  pure $ max 0 (attack - defense - remaining)

-- | For each point of excess damage dealt by Undersea Cave Troll (damage that is dealt
-- beyond the remaining hit points of the character damaged by this attack) discard 1
-- resource from one of your objective-allies or from one of your heroes' resource pools.
-- ponytail: from a hero's pool only, as the attack's damage stands as it is dealt
instance RunMessage UnderseaCaveTroll where
  runMessage msg (UnderseaCaveTroll attrs) = case msg of
    AttackDamage eid defender | eid == attrs.id, character defender, EngagedWith pid <- attrs.placement -> do
      excess <- excessOn eid defender
      replicateM_ excess $ discardResourceFromAHero attrs pid
      UnderseaCaveTroll <$> runMessage msg attrs
    _ -> UnderseaCaveTroll <$> runMessage msg attrs
   where
    character = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
