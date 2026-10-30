module LordOfTheRings.Quest.Cards.DoubleBack (doubleBack) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DoubleBack = DoubleBack QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 per deck, as its card def says.
doubleBack :: QuestCard DoubleBack
doubleBack = quest DoubleBack Cards.doubleBack

-- | Response: When this stage is defeated, reduce each player's threat by 5.
instance RunMessage DoubleBack where
  runMessage msg (DoubleBack attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \pid -> lowerThreatBy attrs pid 5
      DoubleBack <$> runMessage msg attrs
    _ -> DoubleBack <$> runMessage msg attrs
