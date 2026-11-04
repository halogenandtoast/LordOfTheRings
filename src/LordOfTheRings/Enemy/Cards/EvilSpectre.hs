module LordOfTheRings.Enemy.Cards.EvilSpectre (evilSpectre) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Dark))

newtype EvilSpectre = EvilSpectre EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilSpectre :: EnemyCard EvilSpectre
evilSpectre = enemy EvilSpectre Cards.evilSpectre

-- | Forced: When Evil Spectre is destroyed, shuffle it back into the encounter deck if the
-- active location has the Dark trait.
instance HasModifiersFor EvilSpectre where
  getModifiersFor (EvilSpectre a) = do
    dark <- selectAny (IsActiveLocation <> LocationWithTrait Dark)
    when dark $ modifySelf a [ShuffledIntoEncounterDeckWhenDefeated]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the active location has
-- the Dark trait.)
instance RunMessage EvilSpectre where
  runMessage msg e@(EvilSpectre attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      dark <- selectAny (IsActiveLocation <> LocationWithTrait Dark)
      enemyGetsAttack attrs eid (if dark then 2 else 1)
      pure e
    _ -> EvilSpectre <$> runMessage msg attrs
