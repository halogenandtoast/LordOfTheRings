module LordOfTheRings.Location.Cards.WestQuarter (westQuarter) where

import LordOfTheRings.Keyword (Keyword (Siege))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype WestQuarter = WestQuarter LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

westQuarter :: LocationCard WestQuarter
westQuarter = location WestQuarter Cards.westQuarter

-- | While West Quarter is the active location, the current quest gains siege.
instance HasModifiersFor WestQuarter where
  getModifiersFor (WestQuarter a) = when (a.placement == ActiveLocation) do
    getCurrentQuest >>= traverse_ \q -> modified a q [GainKeyword Siege]

-- | Shadow: If this attack destroys a character, that character's controller must return
-- a location he controls with the highest threat to the staging area.
instance RunMessage WestQuarter where
  runMessage msg l@(WestQuarter a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      destroyingReturnsLocation a eid
      pure l
    _ -> WestQuarter <$> runMessage msg a
