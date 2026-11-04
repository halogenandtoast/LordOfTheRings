module LordOfTheRings.Enemy.Cards.DeadThings (deadThings) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (engagedPlayerCannotReduceThreat)
import LordOfTheRings.Trait (Trait (Undead))

newtype DeadThings = DeadThings EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

deadThings :: EnemyCard DeadThings
deadThings = enemy DeadThings Cards.deadThings

-- | The engaged player cannot reduce his threat.
instance HasModifiersFor DeadThings where
  getModifiersFor (DeadThings a) = engagedPlayerCannotReduceThreat a

-- | Forced: After Dead Things destroys a character, the defending player must raise his
-- threat by 3.
instance HasAbilities DeadThings where
  getAbilities (DeadThings a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Raise your threat by 1 for each Undead enemy engaged with you.
instance RunMessage DeadThings where
  runMessage msg e@(DeadThings attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws]) \pid -> encounterRaisesThreat pid 3
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      undead <- length <$> select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId pid))
      when (undead > 0) $ encounterRaisesThreat pid undead
      pure e
    _ -> DeadThings <$> runMessage msg attrs
