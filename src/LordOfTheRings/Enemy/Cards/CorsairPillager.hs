module LordOfTheRings.Enemy.Cards.CorsairPillager (corsairPillager) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype CorsairPillager = CorsairPillager EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairPillager :: EnemyCard CorsairPillager
corsairPillager = enemy CorsairPillager Cards.corsairPillager

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

{- | During the combat phase, Corsair Pillager makes 1 additional attack for each resource
token on it: once the player's enemies have made their attacks, counted then.
Forced: After the players travel to an Aflame location, place 1 resource on Corsair
Pillager for each damage on that location.
-}
instance RunMessage CorsairPillager where
  runMessage msg e@(CorsairPillager attrs) = case msg of
    CheckWindows ws -> do
      for_ [lid | Window After (Traveled lid) <- ws] \lid -> do
        aflame <- getAflame lid
        damage <- getDamage (LocationTarget lid)
        when (aflame > 0 && damage > 0) $ placeResources attrs attrs damage
      CorsairPillager <$> runMessage msg attrs
    -- the game's own attacks against the player go on the queue before this step
    EnemyAttacks pid | EngagedWith pid == attrs.placement -> do
      doStep 1 msg
      pure e
    DoStep 1 (EnemyAttacks pid) | EngagedWith pid == attrs.placement -> do
      replicateM_ (resources attrs) $ immediateAttack pid attrs.id
      pure e
    _ -> CorsairPillager <$> runMessage msg attrs
