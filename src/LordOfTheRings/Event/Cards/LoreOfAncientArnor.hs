module LordOfTheRings.Event.Cards.LoreOfAncientArnor (loreOfAncientArnor) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype LoreOfAncientArnor = LoreOfAncientArnor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A boon of the Angmar Awakened campaign, from The Battle of Carn Dûm.
loreOfAncientArnor :: EventCard LoreOfAncientArnor
loreOfAncientArnor = event LoreOfAncientArnor Cards.loreOfAncientArnor

-- | Action: Each player returns 1 card from their discard pile to their hand (2 cards
-- instead if you are the only player in the game) and shuffles the rest of their discard
-- pile back into their deck. Then, remove this card from the game.
instance RunMessage LoreOfAncientArnor where
  runMessage msg e@(LoreOfAncientArnor attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      players <- getRemainingPlayers
      let n = if length players == 1 then 2 else 1
      for_ players \p -> do
        discarded <- (.discard) <$> getPlayer p
        -- ponytail: two cards are two questions, the same card picked twice returning one
        replicateM_ n $ chooseOrRunOneM p do
          questionLabeled "loreOfAncientArnor.return"
          causedBy attrs
          for_ [c | c <- discarded, c.id /= card.id] \c -> targeting (CardIdTarget c.id) $ returnFromDiscardToHand p c.id
        shuffleDiscardIntoDeck p
      removeSourceCardFromGame (toSource attrs)
      pure e
    _ -> LoreOfAncientArnor <$> runMessage msg attrs
