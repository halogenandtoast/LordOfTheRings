module LordOfTheRings.Quest.Cards.FindAWayDown (findAWayDown) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FindAWayDown = FindAWayDown QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (Mountains of Shadow). Surge, as its card def says.
findAWayDown :: QuestCard FindAWayDown
findAWayDown = quest FindAWayDown Cards.findAWayDown

-- | Each player's threat cannot be reduced.
instance HasModifiersFor FindAWayDown where
  getModifiersFor (FindAWayDown a) = do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotReduceThreat]

{- | Response: When Find a Way Down is defeated, each player reduces his threat by 3. The
players gain by it, so it is taken. It is no longer in play to stop its own reduction, so
the reduction is pushed past the check that would read its modifier.
-}
instance RunMessage FindAWayDown where
  runMessage msg (FindAWayDown attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> push $ LowerThreat p 3
      FindAWayDown <$> runMessage msg attrs
    _ -> FindAWayDown <$> runMessage msg attrs
