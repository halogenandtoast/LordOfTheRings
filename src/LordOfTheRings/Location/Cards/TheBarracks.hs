module LordOfTheRings.Location.Cards.TheBarracks (theBarracks) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (wardensReleased, wardensUnder)

newtype TheBarracks = TheBarracks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Warden under it lies faceup.
theBarracks :: LocationCard TheBarracks
theBarracks = location (TheBarracks . (underneathFaceupL .~ True)) Cards.theBarracks

-- | Forced: After The Barracks becomes the active location, add the enemy under it to the
-- staging area. That enemy cannot take damage this round.
instance HasAbilities TheBarracks where
  getAbilities (TheBarracks a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Forced: When The Barracks is explored, the first player gains control of the objective
-- ally it was guarding: the Captive goes free as its guard leaves play (see 'runCaptive').
instance RunMessage TheBarracks where
  runMessage msg l@(TheBarracks a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ addToStaging (wardensUnder a)
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      wardens <- wardensReleased a
      for_ wardens \e -> roundModifier a e CannotTakeDamage
      pure $ TheBarracks $ a {locationUnderneath = []}
    _ -> TheBarracks <$> runMessage msg a
