module LordOfTheRings.Event.Cards.ATestOfWill (aTestOfWill) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ATestOfWill = ATestOfWill EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Response: Cancel the "when revealed" effects of a card that was just revealed from the
-- encounter deck.
aTestOfWill :: EventCard ATestOfWill
aTestOfWill = eventWith ATestOfWill Cards.aTestOfWill (windowL ?~ CardRevealedWindow When)

instance RunMessage ATestOfWill where
  runMessage msg e@(ATestOfWill attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [t | Window _ (Revealed t) <- windows] \t -> popMessagesMatching (== WhenRevealed t)
      pure e
    _ -> ATestOfWill <$> runMessage msg attrs
