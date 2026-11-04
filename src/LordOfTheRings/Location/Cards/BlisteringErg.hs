module LordOfTheRings.Location.Cards.BlisteringErg (blisteringErg) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (getPlayersStage)

newtype BlisteringErg = BlisteringErg LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blisteringErg :: LocationCard BlisteringErg
blisteringErg = location BlisteringErg Cards.blisteringErg

-- | Forced: After Blistering Erg becomes the active location, each player assigns X damage
-- among characters he controls. X is the stage number of the main quest.
instance HasAbilities BlisteringErg where
  getAbilities (BlisteringErg a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage BlisteringErg where
  runMessage msg l@(BlisteringErg a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      x <- getPlayersStage
      when (x > 0) $ eachPlayer \p -> dealDamageDivided a p x
      pure l
    _ -> BlisteringErg <$> runMessage msg a
