module LordOfTheRings.Enemy.Cards.AngmarOrc (angmarOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype AngmarOrc = AngmarOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

angmarOrc :: EnemyCard AngmarOrc
angmarOrc = enemy AngmarOrc Cards.angmarOrc

-- | When Revealed: Either discard 1 ally from play, or reveal an additional encounter card.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys an ally, return attacking
-- enemy to the staging area.
instance RunMessage AngmarOrc where
  runMessage msg e@(AngmarOrc attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      allies <- select AnyAlly
      chooseOrRunOneM lead do
        questionLabeled "angmarOrc.choose"
        causedBy attrs
        unless (null allies) $ labeled "angmarOrc.discardAlly" $ chooseOneM lead do
          questionLabeled "angmarOrc.chooseAlly"
          causedBy attrs
          targets allies discard
        labeled "angmarOrc.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      allies <- attackDestroyedAllies eid
      unless (null allies) $ placeEnemy eid StagingArea
      pure e
    _ -> AngmarOrc <$> runMessage msg attrs
