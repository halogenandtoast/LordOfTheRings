module LordOfTheRings.Enemy.Cards.AshMountainWerewolf (ashMountainWerewolf) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype AshMountainWerewolf = AshMountainWerewolf EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ashMountainWerewolf :: EnemyCard AshMountainWerewolf
ashMountainWerewolf = enemy AshMountainWerewolf Cards.ashMountainWerewolf

-- | Forced: After Ash Mountain Werewolf attacks you, discard the top card of your deck
-- (top 2 cards instead if this attack destroyed a character).
instance HasAbilities AshMountainWerewolf where
  getAbilities (AshMountainWerewolf a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Shadow: If this attack destroys a character, discard the top 2 cards of your deck.
instance RunMessage AshMountainWerewolf where
  runMessage msg e@(AshMountainWerewolf attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      destroyed <- attackDestroyedACharacter attrs.id
      for_ (take 1 [pid | Window _ (EnemyAttacking _ pid) <- ws]) \pid ->
        discardTopOfDeck pid (if destroyed then 2 else 1)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardTopOfDeck pid 2
      pure e
    _ -> AshMountainWerewolf <$> runMessage msg attrs
