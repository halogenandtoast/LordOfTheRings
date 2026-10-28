module LordOfTheRings.Quest.Cards.GatherInformation (gatherInformation) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype GatherInformation = GatherInformation QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 per deck, as its card def says.
gatherInformation :: QuestCard GatherInformation
gatherInformation = quest GatherInformation Cards.gatherInformation

-- | Response: After this stage is defeated, each player may search his deck for 1 card and
-- add it to his hand. Each player shuffles his deck. Each player's search is the choice:
-- they may take nothing.
instance RunMessage GatherInformation where
  runMessage msg (GatherInformation attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \pid -> do
        deckSize <- length . (.deck) <$> getPlayer pid
        searchDeck pid attrs deckSize AnyCard False
      GatherInformation <$> runMessage msg attrs
    _ -> GatherInformation <$> runMessage msg attrs
