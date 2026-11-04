module LordOfTheRings.Quest.Cards.OverwhelmingDarkness (overwhelmingDarkness) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype OverwhelmingDarkness = OverwhelmingDarkness QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
overwhelmingDarkness :: QuestCard OverwhelmingDarkness
overwhelmingDarkness = quest OverwhelmingDarkness Cards.overwhelmingDarkness

-- | Forced: After a player plays a card, he discards the top card of his deck.
instance HasAbilities OverwhelmingDarkness where
  getAbilities (OverwhelmingDarkness a) = [mkAbility a 1 $ forced $ CardPlayedWindow After AnyCard]

-- | Response: When this stage is defeated, each player shuffles the top 3 cards of his
-- discard pile into his deck. The players gain by it, so it is taken, as Find Water's is.
instance RunMessage OverwhelmingDarkness where
  runMessage msg q@(OverwhelmingDarkness attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (PlayedCard pid _) <- ws] \pid -> discardTopOfDeck pid 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> shuffleTopOfDiscardIntoDeck p 3
      OverwhelmingDarkness <$> runMessage msg attrs
    _ -> OverwhelmingDarkness <$> runMessage msg attrs
