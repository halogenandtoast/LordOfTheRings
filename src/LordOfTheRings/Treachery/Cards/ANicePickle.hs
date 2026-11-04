module LordOfTheRings.Treachery.Cards.ANicePickle (aNicePickle) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ANicePickle = ANicePickle TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aNicePickle :: TreacheryCard ANicePickle
aNicePickle = treachery ANicePickle Cards.aNicePickle

-- | Doomed 1.
-- When Revealed: Place the top X cards of the encounter discard pile on the bottom of the
-- encounter deck. X is equal to twice the number of players in the game.
-- Shadow: Shuffle this card into the encounter deck.
instance RunMessage ANicePickle where
  runMessage msg t@(ANicePickle attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      moveDiscardToBottomOfEncounterDeck (2 * players)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      shuffleShadowCardIntoEncounterDeck eid (MkEncounterCard cid (toCardCode attrs))
      pure t
    _ -> ANicePickle <$> runMessage msg attrs
