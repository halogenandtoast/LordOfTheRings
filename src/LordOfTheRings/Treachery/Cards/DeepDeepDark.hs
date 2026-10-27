module LordOfTheRings.Treachery.Cards.DeepDeepDark (deepDeepDark) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Trait (Trait (Nameless))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeepDeepDark = DeepDeepDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepDeepDark :: TreacheryCard DeepDeepDark
deepDeepDark = treachery DeepDeepDark Cards.deepDeepDark

-- | Doomed 1. Surge.
-- When Revealed: Attach 1 card from the top of the first player's deck to each Nameless
-- enemy in play, if able.
-- Shadow: If attacking enemy is Nameless, the defending player must discard his hand.
instance RunMessage DeepDeepDark where
  runMessage msg t@(DeepDeepDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      deck <- (.deck) <$> getPlayer lead
      nameless <- select (EnemyWithTrait Nameless)
      for_ (zip nameless [pc | PlayerCard pc <- deck]) \(eid, pc) -> do
        removeFromDeck lead pc.id
        attachFacedown eid pc
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Nameless) $ discardHand pid
      pure t
    _ -> DeepDeepDark <$> runMessage msg attrs
