module LordOfTheRings.Location.Cards.EastQuarter (eastQuarter) where

import LordOfTheRings.Keyword (Keyword (Battle))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype EastQuarter = EastQuarter LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastQuarter :: LocationCard EastQuarter
eastQuarter = location EastQuarter Cards.eastQuarter

-- | While East Quarter is the active location, the current quest gains battle.
instance HasModifiersFor EastQuarter where
  getModifiersFor (EastQuarter a) = when (a.placement == ActiveLocation) do
    getCurrentQuest >>= traverse_ \q -> modified a q [GainKeyword Battle]

-- | Shadow: If this attack destroys a character, that character's controller must return
-- a location he controls with the highest threat to the staging area.
instance RunMessage EastQuarter where
  runMessage msg l@(EastQuarter a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      destroyedReturnLocations a eid
      pure l
    _ -> EastQuarter <$> runMessage msg a
