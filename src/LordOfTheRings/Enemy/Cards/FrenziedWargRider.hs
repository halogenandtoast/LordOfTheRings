module LordOfTheRings.Enemy.Cards.FrenziedWargRider (frenziedWargRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (currentProgress)

newtype FrenziedWargRider = FrenziedWargRider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frenziedWargRider :: EnemyCard FrenziedWargRider
frenziedWargRider = enemy FrenziedWargRider Cards.frenziedWargRider

instance HasAbilities FrenziedWargRider where
  getAbilities (FrenziedWargRider a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage FrenziedWargRider where
  runMessage msg e@(FrenziedWargRider a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      stages <- getMainQuests
      for_ stages \q -> removeTokens a q Progress 1
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      progress <- currentProgress
      for_ defenders \d -> dealDamage (CardIdSource cid) d (if progress == 0 then 2 else 1)
      pure e
    _ -> FrenziedWargRider <$> runMessage msg a
