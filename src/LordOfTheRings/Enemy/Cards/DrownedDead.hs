module LordOfTheRings.Enemy.Cards.DrownedDead (drownedDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DrownedDead = DrownedDead EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drownedDead :: EnemyCard DrownedDead
drownedDead = enemy DrownedDead Cards.drownedDead

-- | Forced: When Drowned Dead attacks and destroys an ally, place that ally on the bottom
-- of its owner's deck: once it is destroyed, from its owner's discard pile.
instance HasAbilities DrownedDead where
  getAbilities (DrownedDead a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: If this attack destroys an ally, place that ally on the bottom of its owner's
-- deck.
instance RunMessage DrownedDead where
  runMessage msg e@(DrownedDead attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      let destroyed = [t | Window _ (CharacterDestroyedByAttack _ t@(AllyTarget _) _) <- ws]
      allies <- filter ((`elem` destroyed) . (.target)) <$> attackDestroyedAllies attrs.id
      traverse_ (allyToBottomOfDeck . (.card)) allies
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      traverse_ (allyToBottomOfDeck . (.card)) =<< attackDestroyedAllies eid
      pure e
    _ -> DrownedDead <$> runMessage msg attrs
