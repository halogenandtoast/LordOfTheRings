module LordOfTheRings.Enemy.Cards.LieutenantOfOrthanc (lieutenantOfOrthanc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype LieutenantOfOrthanc = LieutenantOfOrthanc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lieutenantOfOrthanc :: EnemyCard LieutenantOfOrthanc
lieutenantOfOrthanc = enemy LieutenantOfOrthanc Cards.lieutenantOfOrthanc

-- | Forced: After Lieutenant of Orthanc attacks and destroys a character, that character's
-- controller discards 1 random card from his hand.
instance HasAbilities LieutenantOfOrthanc where
  getAbilities (LieutenantOfOrthanc a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage LieutenantOfOrthanc where
  runMessage msg e@(LieutenantOfOrthanc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (CharacterDestroyedByAttack eid _ pid) <- ws, eid == attrs.id] \pid ->
        discardRandomFromHand pid 1
      pure e
    _ -> LieutenantOfOrthanc <$> runMessage msg attrs
