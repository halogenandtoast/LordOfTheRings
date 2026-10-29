module LordOfTheRings.Enemy.Cards.EvilCrow (evilCrow) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)

newtype EvilCrow = EvilCrow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilCrow :: EnemyCard EvilCrow
evilCrow = enemy EvilCrow Cards.evilCrow

-- | Forced: When Evil Crow engages a player, that player takes 1 hidden card.
instance HasAbilities EvilCrow where
  getAbilities (EvilCrow a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

-- | Shadow: Take 1 hidden card.
instance RunMessage EvilCrow where
  runMessage msg e@(EvilCrow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] takeHiddenCard
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      takeHiddenCard pid
      pure e
    _ -> EvilCrow <$> runMessage msg attrs
