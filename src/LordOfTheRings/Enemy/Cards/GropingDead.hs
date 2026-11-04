module LordOfTheRings.Enemy.Cards.GropingDead (gropingDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (engagedPlayerCannotReduceThreat, placeMireOnActiveLocation)

newtype GropingDead = GropingDead EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

gropingDead :: EnemyCard GropingDead
gropingDead = enemy GropingDead Cards.gropingDead

-- | The engaged player cannot reduce his threat.
instance HasModifiersFor GropingDead where
  getModifiersFor (GropingDead a) = engagedPlayerCannotReduceThreat a

-- | Forced: After Groping Dead destroys a character, place 1 mire token on the active
-- location.
instance HasAbilities GropingDead where
  getAbilities (GropingDead a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: If this attack destroys a character, place 1 mire token on the active location.
instance RunMessage GropingDead where
  runMessage msg e@(GropingDead attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeMireOnActiveLocation attrs
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeMireOnActiveLocation attrs
      pure e
    _ -> GropingDead <$> runMessage msg attrs
