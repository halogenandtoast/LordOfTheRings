module LordOfTheRings.Quest.Cards.SendForAid (sendForAid) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SendForAid = SendForAid QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 per deck, as its card def says.
sendForAid :: QuestCard SendForAid
sendForAid = quest SendForAid Cards.sendForAid

-- | Response: When this stage is defeated, each player may search the top 10 cards of his
-- deck for an ally and put it into play under his control. Each player who did shuffles
-- his deck.
instance RunMessage SendForAid where
  runMessage msg (SendForAid attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \pid -> searchDeckIntoPlay pid attrs 10 (CardWithType AllyType) 1 []
      SendForAid <$> runMessage msg attrs
    _ -> SendForAid <$> runMessage msg attrs
