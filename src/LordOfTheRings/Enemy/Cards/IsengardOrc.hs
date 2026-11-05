module LordOfTheRings.Enemy.Cards.IsengardOrc (isengardOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype IsengardOrc = IsengardOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Archery 1, as its card def says.
isengardOrc :: EnemyCard IsengardOrc
isengardOrc = enemy IsengardOrc Cards.isengardOrc

{- | When Revealed: Either discard 1 random card from your hand, or reveal an encounter card.
A player with an empty hand reveals one, as an option must be done in full (FAQ 1.44).
Shadow: Until the end of the round, attacking enemy gets +2 defense.
-}
instance RunMessage IsengardOrc where
  runMessage msg e@(IsengardOrc attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      hand <- (.hand) <$> getPlayer you
      chooseOrRunOneM you do
        questionLabeled "isengardOrc.choose"
        causedBy attrs
        when (notNull hand) $ labeled "isengardOrc.discard" $ discardRandomFromHand you 1
        labeled "isengardOrc.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier (CardIdSource cid) eid (DefenseModifier 2)
      pure e
    _ -> IsengardOrc <$> runMessage msg attrs
