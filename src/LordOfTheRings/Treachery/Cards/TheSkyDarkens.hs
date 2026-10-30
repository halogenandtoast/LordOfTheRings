module LordOfTheRings.Treachery.Cards.TheSkyDarkens (theSkyDarkens) where

import LordOfTheRings.Card (Card (PlayerCard), toCardCode)
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheSkyDarkens = TheSkyDarkens TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSkyDarkens :: TreacheryCard TheSkyDarkens
theSkyDarkens = treachery TheSkyDarkens Cards.theSkyDarkens

-- | When Revealed: Each player must discard a random card from his hand. Each player then
-- searches his deck for each other copy of the card he discarded, and discards those as
-- well. Shuffle each searched deck.
-- Shadow: Discard a random card from the defending player's hand.
instance RunMessage TheSkyDarkens where
  runMessage msg t@(TheSkyDarkens attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        player <- getPlayer p
        discarded <- headMay <$> shuffleM (discardableHand player)
        for_ discarded \card -> do
          discardFromHand p card
          for_ [pc | PlayerCard pc <- player.deck, toCardCode pc == toCardCode card] (discardFromDeck p)
          shufflePlayerDeck p
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure t
    _ -> TheSkyDarkens <$> runMessage msg attrs
