module LordOfTheRings.Enemy.Cards.AngmarMarauder (angmarMarauder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype AngmarMarauder = AngmarMarauder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

angmarMarauder :: EnemyCard AngmarMarauder
angmarMarauder = enemy AngmarMarauder Cards.angmarMarauder

-- | Forced: After Angmar Marauder attacks and destroys an ally, return it to the staging
-- area: once its attack is over.
instance HasAbilities AngmarMarauder where
  getAbilities (AngmarMarauder a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Attacking enemy gets +1 attack for each ally the defending player controls.
instance RunMessage AngmarMarauder where
  runMessage msg e@(AngmarMarauder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      when (notNull [() | Window _ (CharacterDestroyedByAttack _ (AllyTarget _) _) <- ws])
        $ returnToStagingAfterAttack attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      allies <- length <$> select (AllyControlledBy (PlayerWithId pid))
      when (allies > 0) $ enemyGetsAttack (CardIdSource cid) eid allies
      pure e
    _ -> AngmarMarauder <$> runMessage msg attrs
