module LordOfTheRings.Ally.Cards.Haleth (haleth) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))

newtype Haleth = Haleth AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

haleth :: AllyCard Haleth
haleth = ally Haleth Cards.haleth

-- | Response: After Haleth quests successfully, search the top 5 cards of your deck for an
-- attachment and add it to your hand. Shuffle your deck.
instance HasAbilities Haleth where
  getAbilities (Haleth a) =
    [restricted a 1 (exists $ AllyWithId a.id <> CommittedAlly) $ response $ QuestedSuccessfullyWindow After]

instance RunMessage Haleth where
  runMessage msg s@(Haleth a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithType AttachmentType) False
      pure s
    _ -> Haleth <$> runMessage msg a
