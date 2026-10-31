module LordOfTheRings.Location.Cards.HideousDepths (hideousDepths) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (isTentacleEnemy)

newtype HideousDepths = HideousDepths LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hideousDepths :: LocationCard HideousDepths
hideousDepths = location HideousDepths Cards.hideousDepths

-- | With no Tentacle enemy in the encounter discard pile, its Travel cost cannot be paid.
-- ponytail: "Forced: After a player looks at one or more cards in the encounter deck,
-- shuffle the encounter deck." is not scripted: no window tells a look at the deck yet.
instance HasModifiersFor HideousDepths where
  getModifiersFor (HideousDepths a) = do
    tentacles <- filter isTentacleEnemy <$> getEncounterDiscard
    when (null tentacles) $ modified a a [CannotBeTraveledTo]

-- | Travel: Reveal the topmost Tentacle enemy in the encounter discard pile to travel here.
instance RunMessage HideousDepths where
  runMessage msg (HideousDepths a) = case msg of
    TravelTo lid | lid == a.id -> do
      tentacles <- filter isTentacleEnemy <$> getEncounterDiscard
      for_ (take 1 tentacles) revealFromEncounterDiscard
      HideousDepths <$> runMessage msg a
    _ -> HideousDepths <$> runMessage msg a
