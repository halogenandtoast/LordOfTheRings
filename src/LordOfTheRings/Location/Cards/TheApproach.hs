module LordOfTheRings.Location.Cards.TheApproach (theApproach) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (runBattleground)

newtype TheApproach = TheApproach LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theApproach :: LocationCard TheApproach
theApproach = location TheApproach Cards.theApproach

-- | If The Approach has 7 or more damage, remove it from the game (do not collect its victory
-- points).
-- Response: After The Approach leaves play as an explored location, remove stage 3 from the
-- quest deck, if able.
instance RunMessage TheApproach where
  runMessage msg (TheApproach a) = TheApproach <$> runBattleground Quests.breakthroughAtTheApproach msg a
