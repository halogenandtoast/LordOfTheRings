module LordOfTheRings.Enemy.Cards.MorgulOrc (morgulOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MorgulOrc = MorgulOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulOrc :: EnemyCard MorgulOrc
morgulOrc = enemy MorgulOrc Cards.morgulOrc

{- | Peril.
When Revealed: Either discard 1 random card from your hand, or reveal an additional
encounter card. With no card in hand, the discard cannot be done and the card is revealed.
Shadow: If this attack destroys a character, discard 1 random card from your hand.
-}
instance RunMessage MorgulOrc where
  runMessage msg e@(MorgulOrc attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      hand <- (.hand) <$> getPlayer you
      chooseOrRunOneM you do
        questionLabeled "morgulOrc.choose"
        causedBy attrs
        unless (null hand) $ labeled "morgulOrc.discard" $ discardRandomFromHand you 1
        labeled "morgulOrc.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure e
    _ -> MorgulOrc <$> runMessage msg attrs
