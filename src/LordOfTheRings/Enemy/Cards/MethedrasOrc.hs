module LordOfTheRings.Enemy.Cards.MethedrasOrc (methedrasOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (shuffleRandomCardIntoOutOfPlayDeck)

newtype MethedrasOrc = MethedrasOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

methedrasOrc :: EnemyCard MethedrasOrc
methedrasOrc = enemy MethedrasOrc Cards.methedrasOrc

-- | Forced: When Methedras Orc attacks, the defending player shuffles 1 random card from
-- his hand into his out-of-play deck.
instance HasAbilities MethedrasOrc where
  getAbilities (MethedrasOrc a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: If this attack destroys a character, remove 1 time counter from the current
-- quest.
instance RunMessage MethedrasOrc where
  runMessage msg e@(MethedrasOrc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] shuffleRandomCardIntoOutOfPlayDeck
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure e
    _ -> MethedrasOrc <$> runMessage msg attrs
