module LordOfTheRings.Enemy.Cards.UmbarSentry (umbarSentry) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UmbarSentry = UmbarSentry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge. Archery 1. As its card def says.
umbarSentry :: EnemyCard UmbarSentry
umbarSentry = enemy UmbarSentry Cards.umbarSentry

-- | Forced: When Umbar Sentry attacks, either remove 1 progress token from the main quest,
-- or it gets +2 attack for this attack. The attacked player chooses; the removal is
-- offered only when there is progress to remove (FAQ 1.44).
instance HasAbilities UmbarSentry where
  getAbilities (UmbarSentry a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage UmbarSentry where
  runMessage msg e@(UmbarSentry attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyAttacking _ pid) <- ws]) \pid ->
        chooseOrRunOneM pid do
          questionLabeled "umbarSentry.choose"
          causedBy attrs
          removeMainQuestProgressLabeled attrs 1
          labeled "umbarSentry.attack" $ enemyGetsAttack attrs attrs.id 2
      pure e
    _ -> UmbarSentry <$> runMessage msg attrs
