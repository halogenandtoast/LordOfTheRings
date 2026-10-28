module LordOfTheRings.Location.Cards.TheBanks (theBanks) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (runBattleground)

newtype TheBanks = TheBanks LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBanks :: LocationCard TheBanks
theBanks = location TheBanks Cards.theBanks

-- | If The Banks has 3 or more damage, remove it from the game (do not collect its victory
-- points).
-- Response: After The Banks leaves play as an explored location, remove stage 2 from the
-- quest deck, if able.
instance RunMessage TheBanks where
  runMessage msg (TheBanks a) = TheBanks <$> runBattleground Quests.reinforcingTheBanks msg a
