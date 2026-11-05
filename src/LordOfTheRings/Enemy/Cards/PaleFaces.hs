module LordOfTheRings.Enemy.Cards.PaleFaces (paleFaces) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (engagedPlayerCannotReduceThreat)

newtype PaleFaces = PaleFaces EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

paleFaces :: EnemyCard PaleFaces
paleFaces = enemy PaleFaces Cards.paleFaces

instance HasModifiersFor PaleFaces where
  getModifiersFor (PaleFaces a) = engagedPlayerCannotReduceThreat a

instance HasAbilities PaleFaces where
  getAbilities (PaleFaces a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage PaleFaces where
  runMessage msg e@(PaleFaces a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == a.id]) \pid -> encounterRaisesThreat pid 4
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      n <- sum <$> (traverse getMire =<< select IsActiveLocation)
      enemyGetsAttack a eid n
      pure e
    _ -> PaleFaces <$> runMessage msg a
