module LordOfTheRings.Quest.Cards.Isolated (isolated) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype Isolated = Isolated QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
isolated :: QuestCard Isolated
isolated = quest Isolated Cards.isolated

-- | Players cannot draw cards by card effects.
instance HasModifiersFor Isolated where
  getModifiersFor (Isolated a) = do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotDrawCardsFromCardEffects]

-- | Forced: When this stage is defeated, each player draws 1 card. Isolated still bars
-- card effects' draws as it is defeated, so the draw is pushed as it is.
instance RunMessage Isolated where
  runMessage msg (Isolated attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> push $ DrawCards $ DrawingFrom p 1 (toSource attrs)
      Isolated <$> runMessage msg attrs
    _ -> Isolated <$> runMessage msg attrs
