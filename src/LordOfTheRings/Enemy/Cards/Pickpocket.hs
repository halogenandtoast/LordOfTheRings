module LordOfTheRings.Enemy.Cards.Pickpocket (pickpocket) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Pickpocket = Pickpocket EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pickpocket :: EnemyCard Pickpocket
pickpocket = enemy Pickpocket Cards.pickpocket

-- | Forced: When Pickpocket attacks, the defending player discards 1 resource from one of
-- his heroes' resource pools and 1 card at random from his hand.
instance HasAbilities Pickpocket where
  getAbilities (Pickpocket a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Defending player discards 1 of his attachments. (Discard all of his
-- attachments instead if undefended.)
instance RunMessage Pickpocket where
  runMessage msg e@(Pickpocket attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> do
        discardResourceFromAHero attrs pid
        discardRandomFromHand pid 1
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      ifUndefended defenders (for_ attachments (discard attrs)) (chooseAndDiscard (EnemySource eid) pid attachments)
      pure e
    _ -> Pickpocket <$> runMessage msg attrs
