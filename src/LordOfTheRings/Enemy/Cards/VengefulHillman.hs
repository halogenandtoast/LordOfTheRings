module LordOfTheRings.Enemy.Cards.VengefulHillman (vengefulHillman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype VengefulHillman = VengefulHillman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vengefulHillman :: EnemyCard VengefulHillman
vengefulHillman = enemy VengefulHillman Cards.vengefulHillman

-- | Forced: After Vengeful Hillman attacks and destroys a character, remove 1 progress from
-- the current quest card for each card in the defending player's hand.
instance HasAbilities VengefulHillman where
  getAbilities (VengefulHillman a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage VengefulHillman where
  runMessage msg e@(VengefulHillman attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (nub [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws]) \pid -> do
        n <- length . (.hand) <$> getPlayer pid
        when (n > 0) $ removeProgressFromQuest attrs n
      pure e
    _ -> VengefulHillman <$> runMessage msg attrs
