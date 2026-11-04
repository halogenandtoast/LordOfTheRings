module LordOfTheRings.Enemy.Cards.CandleBearer (candleBearer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (engagedPlayerCannotReduceThreat)

newtype CandleBearer = CandleBearer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

candleBearer :: EnemyCard CandleBearer
candleBearer = enemy CandleBearer Cards.candleBearer

-- | The engaged player cannot reduce his threat.
instance HasModifiersFor CandleBearer where
  getModifiersFor (CandleBearer a) = engagedPlayerCannotReduceThreat a

-- | Forced: After Candle-bearer destroys a character, it cannot take damage until the end
-- of the round.
instance HasAbilities CandleBearer where
  getAbilities (CandleBearer a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Until the end of the round, attacking enemy cannot take damage.
instance RunMessage CandleBearer where
  runMessage msg e@(CandleBearer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs CannotTakeDamage
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure e
    _ -> CandleBearer <$> runMessage msg attrs
