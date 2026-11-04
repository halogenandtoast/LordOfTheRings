module LordOfTheRings.Enemy.Cards.GuardDogs (guardDogs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GuardDogs = GuardDogs EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
guardDogs :: EnemyCard GuardDogs
guardDogs = enemy GuardDogs Cards.guardDogs

-- | Forced: After Guard Dogs engages you, deal 1 damage to a character you control.
instance HasAbilities GuardDogs where
  getAbilities (GuardDogs a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Put Guard Dogs into play engaged with you.
instance RunMessage GuardDogs where
  runMessage msg e@(GuardDogs attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageToOneCharacter attrs pid 1
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      putShadowEnemyIntoPlay eid cid pid
      pure e
    _ -> GuardDogs <$> runMessage msg attrs
