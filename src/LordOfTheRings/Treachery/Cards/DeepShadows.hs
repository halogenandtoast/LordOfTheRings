module LordOfTheRings.Treachery.Cards.DeepShadows (deepShadows) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeepShadows = DeepShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 2.
deepShadows :: TreacheryCard DeepShadows
deepShadows = treachery DeepShadows Cards.deepShadows

{- | When Revealed: Each player must choose: either discard your hand (with at least 3 cards
in it), or reveal an encounter card. A player with fewer than 3 cards in hand reveals one.
Shadow: Attacking enemy makes an additional attack after this one.
-}
instance RunMessage DeepShadows where
  runMessage msg t@(DeepShadows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        hand <- (.hand) <$> getPlayer pid
        chooseOrRunOneM pid do
          questionLabeled "deepShadows.choose"
          causedBy attrs
          when (length hand >= 3) $ labeled "deepShadows.discardHand" $ discardHand pid
          labeled "deepShadows.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> DeepShadows <$> runMessage msg attrs
