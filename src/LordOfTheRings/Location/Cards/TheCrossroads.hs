module LordOfTheRings.Location.Cards.TheCrossroads (theCrossroads) where

import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)

newtype TheCrossroads = TheCrossroads LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrossroads :: LocationCard TheCrossroads
theCrossroads = location TheCrossroads Cards.theCrossroads

-- | The current quest card gains siege (and loses battle) (FAQ errata). X is the number of
-- players in the game.
instance HasModifiersFor TheCrossroads where
  getModifiersFor (TheCrossroads a) = do
    players <- length <$> getRemainingPlayers
    modified a a [ThreatModifier players]
    getCurrentQuest >>= traverse_ \q -> modified a q [GainKeyword Siege, LoseKeyword Battle]

-- | Travel: The first player must take 1 hidden card to travel here.
instance RunMessage TheCrossroads where
  runMessage msg l@(TheCrossroads a) = case msg of
    TravelTo lid | lid == a.id -> do
      takeHiddenCard =<< getLead
      pure l
    _ -> TheCrossroads <$> runMessage msg a
