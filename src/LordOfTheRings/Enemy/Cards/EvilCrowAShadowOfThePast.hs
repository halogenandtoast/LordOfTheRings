module LordOfTheRings.Enemy.Cards.EvilCrowAShadowOfThePast (evilCrowAShadowOfThePast) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (failedHideTestThisRound)

newtype EvilCrowAShadowOfThePast = EvilCrowAShadowOfThePast EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilCrowAShadowOfThePast :: EnemyCard EvilCrowAShadowOfThePast
evilCrowAShadowOfThePast = enemy EvilCrowAShadowOfThePast Cards.evilCrowAShadowOfThePast

-- | While Evil Crow is engaged with a player, that player adds 1 to the Hide value for each
-- Hide test he makes.
instance HasModifiersFor EvilCrowAShadowOfThePast where
  getModifiersFor (EvilCrowAShadowOfThePast a) = case a.placement of
    EngagedWith pid -> modified a pid [HideValue 1]
    _ -> pure ()

-- | Shadow: If you have failed a Hide test this round, this attack is considered undefended.
instance RunMessage EvilCrowAShadowOfThePast where
  runMessage msg e@(EvilCrowAShadowOfThePast attrs) = case msg of
    ResolveShadowEffect cid eid pid (_ : _) | cid == attrs.cardId -> do
      whenM (failedHideTestThisRound pid) $ makeUndefended pid eid
      pure e
    _ -> EvilCrowAShadowOfThePast <$> runMessage msg attrs
