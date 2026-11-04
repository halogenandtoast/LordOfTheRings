module LordOfTheRings.Ally.Cards.MessengerRaven (messengerRaven) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (..))
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype MessengerRaven = MessengerRaven AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

messengerRaven :: AllyCard MessengerRaven
messengerRaven = ally MessengerRaven Cards.messengerRaven

-- | Cannot attack, defend, or have attachments.
instance HasModifiersFor MessengerRaven where
  getModifiersFor (MessengerRaven a) = modifySelf a [CannotAttackOrDefend, CannotHaveAttachments]

-- | Refresh Action: Return Messenger Raven to its owner's hand and name a card type. Choose
-- a player to look at the top card of his deck. If that card is the named type, add it to
-- its owner's hand. If it's not, discard it.
instance HasAbilities MessengerRaven where
  getAbilities (MessengerRaven a) = [restricted a 1 (DuringPhase RefreshPhase) $ ActionAbility Free]

instance RunMessage MessengerRaven where
  runMessage msg s@(MessengerRaven a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      returnToHand a
      chooseOneM pid do
        questionLabeled "messengerRaven.name"
        causedBy a
        for_
          [ ("messengerRaven.ally", AllyType)
          , ("messengerRaven.attachment", AttachmentType)
          , ("messengerRaven.event", EventType)
          , ("messengerRaven.sideQuest", PlayerSideQuestType)
          ]
          \(key, named) -> labeled key $ choosePlayerM pid a \p ->
            getPlayer p >>= \player -> for_ (headMay player.deck) \top ->
              if top.cardType == named then takeFromDeck p top else discardTopOfDeck p 1
      pure s
    _ -> MessengerRaven <$> runMessage msg a
