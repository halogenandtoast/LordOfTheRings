module LordOfTheRings.Event.Cards.MessageFromElrond (messageFromElrond) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype MessageFromElrond = MessageFromElrond EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. That player may choose 1 card from his hand and add it to
-- another player's hand. At the end of the round, if the chosen card is in that player's
-- hand or in play, shuffle it into its owner's deck.
messageFromElrond :: EventCard MessageFromElrond
messageFromElrond = event MessageFromElrond Cards.messageFromElrond

instance RunMessage MessageFromElrond where
  runMessage msg e@(MessageFromElrond attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> do
        hand <- (.hand) <$> getPlayer p
        others <- filter (/= p) <$> getRemainingPlayers
        chooseOneM p do
          questionLabeled "messageFromElrond.choose"
          causedBy attrs
          for_ hand \c -> targeting (CardIdTarget c.id) do
            chooseOrRunOneM p do
              questionLabeled "messageFromElrond.givePlayer"
              causedBy attrs
              targets others \o -> do
                push $ GiveFromHand p c.id o
                roundModifier attrs (CardIdTarget c.id) ShuffleIntoOwnersDeckAtEndOfRound
          labeled "pass" (pure ())
      pure e
    _ -> MessageFromElrond <$> runMessage msg attrs
