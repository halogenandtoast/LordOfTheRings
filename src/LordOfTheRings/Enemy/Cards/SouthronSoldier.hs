module LordOfTheRings.Enemy.Cards.SouthronSoldier (southronSoldier) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SouthronSoldier = SouthronSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronSoldier :: EnemyCard SouthronSoldier
southronSoldier = enemy SouthronSoldier Cards.southronSoldier

-- | Forced: After Southron Soldier engages you, either remove 2 progress tokens from the
-- main quest, or Southron Soldier makes an immediate attack. The removal is offered only
-- when it can be made in full (FAQ 1.44).
instance HasAbilities SouthronSoldier where
  getAbilities (SouthronSoldier a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

{- | Shadow: Choose an enemy in the staging area. Engage it and deal it a shadow card. It
engages the defending player during his attacks, so it attacks this step as well, with the
shadow card it is dealt: read back as a later step of the shadow, once it is engaged, as
Murzag's revealed enemy is, the enemy carried in place of the defenders.
-}
instance RunMessage SouthronSoldier where
  runMessage msg e@(SouthronSoldier attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "southronSoldier.choose"
          causedBy attrs
          removeMainQuestProgressLabeled attrs 2
          labeled "southronSoldier.attack" $ immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      staged <- select EnemyInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "southronSoldier.engage"
        causedBy attrs
        targets staged \s -> do
          engageEnemy pid s
          dealShadowCard s
          doStep 1 (ResolveShadowEffect cid eid pid [toTarget s])
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid [EnemyTarget s]) | cid == attrs.cardId -> do
      attacksThisStep pid s
      pure e
    _ -> SouthronSoldier <$> runMessage msg attrs
