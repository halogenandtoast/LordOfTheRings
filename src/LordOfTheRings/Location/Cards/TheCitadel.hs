module LordOfTheRings.Location.Cards.TheCitadel (theCitadel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (runBattleground)

newtype TheCitadel = TheCitadel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCitadel :: LocationCard TheCitadel
theCitadel = location TheCitadel Cards.theCitadel

-- | If The Citadel has 11 or more damage, remove it from the game (do not collect its victory
-- points).
-- Response: After The Citadel leaves play as an explored location, remove stage 4 from the
-- quest deck, if able.
instance RunMessage TheCitadel where
  runMessage msg (TheCitadel a) = TheCitadel <$> runBattleground Quests.breakthroughAtTheCitadel msg a
