module LordOfTheRings.Enemy.Cards.Dwimmerlaik (dwimmerlaik) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateDestroyedBy)

newtype Dwimmerlaik = Dwimmerlaik EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwimmerlaik :: EnemyCard Dwimmerlaik
dwimmerlaik = enemy Dwimmerlaik Cards.dwimmerlaik

-- | Forced: After Dwimmerlaik attacks and destroys a character, the defending player
-- reanimates that character and deals it 1 shadow card.
instance HasAbilities Dwimmerlaik where
  getAbilities (Dwimmerlaik a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: If this attack destroys a character, reanimate that character and deal it 1
-- shadow card.
instance RunMessage Dwimmerlaik where
  runMessage msg e@(Dwimmerlaik attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      reanimateDestroyedBy attrs.id $ Just [t | Window _ (CharacterDestroyedByAttack _ t _) <- ws]
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      reanimateDestroyedBy eid Nothing
      pure e
    _ -> Dwimmerlaik <$> runMessage msg attrs
