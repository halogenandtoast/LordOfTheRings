module LordOfTheRings.Quest.Cards.ThePowerOfAngmar (thePowerOfAngmar) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThePowerOfAngmar = ThePowerOfAngmar QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
thePowerOfAngmar :: QuestCard ThePowerOfAngmar
thePowerOfAngmar = quest ThePowerOfAngmar Cards.thePowerOfAngmar

-- | Cards in each player's discard pile cannot leave that player's discard pile by player
-- card effect.
instance HasModifiersFor ThePowerOfAngmar where
  getModifiersFor (ThePowerOfAngmar a) = modified a GameTarget [DiscardPilesLocked]

-- | Forced: After this stage is defeated, each player shuffles the top 5 cards of his
-- discard pile back into his deck.
instance RunMessage ThePowerOfAngmar where
  runMessage msg (ThePowerOfAngmar attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \pid -> shuffleTopOfDiscardIntoDeck pid 5
      ThePowerOfAngmar <$> runMessage msg attrs
    _ -> ThePowerOfAngmar <$> runMessage msg attrs
