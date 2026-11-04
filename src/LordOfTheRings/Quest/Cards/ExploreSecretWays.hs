module LordOfTheRings.Quest.Cards.ExploreSecretWays (exploreSecretWays) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ExploreSecretWays = ExploreSecretWays QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 copy of Explore Secret Ways in the victory display, as its
-- card def says.
exploreSecretWays :: QuestCard ExploreSecretWays
exploreSecretWays = quest ExploreSecretWays Cards.exploreSecretWays

-- | While this side quest is in the victory display, each location in the staging area
-- with the same title as the active location does not contribute its threat to the total
-- threat of the staging area.
instance HasModifiersFor ExploreSecretWays where
  getModifiersFor (ExploreSecretWays a) = whenM (inVictoryDisplay a.cardId) do
    locations <- select (LocationInStagingArea <> LocationWithActiveLocationTitle)
    for_ locations \l -> modified a l [DoesNotContributeThreat]
