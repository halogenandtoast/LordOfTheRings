module LordOfTheRings.Quest.Cards.HoldTheDoor (holdTheDoor) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype HoldTheDoor = HoldTheDoor QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
holdTheDoor :: QuestCard HoldTheDoor
holdTheDoor = quest HoldTheDoor Cards.holdTheDoor

-- | Each player's threat cannot be reduced: not once it has its quest points in progress,
-- defeated, so its own Response may reduce it.
instance HasModifiersFor HoldTheDoor where
  getModifiersFor (HoldTheDoor a) =
    unlessM (hasEnoughProgress a) do
      players <- getRemainingPlayers
      for_ players \p -> modified a (PlayerTarget p) [CannotReduceThreat]

-- | Response: When Hold the Door is defeated, reduce each player's threat by 5. The
-- players gain by it, so it is taken, as Seek Shade's is.
instance RunMessage HoldTheDoor where
  runMessage msg (HoldTheDoor attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> lowerThreatBy attrs p 5
      HoldTheDoor <$> runMessage msg attrs
    _ -> HoldTheDoor <$> runMessage msg attrs
