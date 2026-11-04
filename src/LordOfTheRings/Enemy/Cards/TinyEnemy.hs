module LordOfTheRings.Enemy.Cards.TinyEnemy (tinyEnemy) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

-- | Tiny, enemy side faceup: Urdug's Gambit flips his objective-ally side
-- ("LordOfTheRings.Ally.Cards.Tiny") to it.
newtype TinyEnemy = TinyEnemy EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

tinyEnemy :: EnemyCard TinyEnemy
tinyEnemy = enemy TinyEnemy Cards.tinyEnemy

urdug :: EnemyMatcher
urdug = EnemyWithCardCode (toCardCode Cards.urdugEnemy)

-- | Immune to player card effects. Urdug cannot take damage.
instance HasModifiersFor TinyEnemy where
  getModifiersFor (TinyEnemy a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    traverse_ (\u -> modified a u [CannotTakeDamage]) =<< select urdug

-- | Forced: After Urdug engages a player, Tiny engages that player.
instance HasAbilities TinyEnemy where
  getAbilities (TinyEnemy a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After urdug]

instance RunMessage TinyEnemy where
  runMessage msg e@(TinyEnemy attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window After (EnemyEngaged _ pid) <- ws]) \pid ->
        unless (attrs.placement == EngagedWith pid) $ engageEnemy pid attrs.id
      pure e
    _ -> TinyEnemy <$> runMessage msg attrs
