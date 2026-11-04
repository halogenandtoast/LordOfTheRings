module LordOfTheRings.Location.Cards.TheStorehouse (theStorehouse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (wardensReleased, wardensUnder)

newtype TheStorehouse = TheStorehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Warden under it lies faceup.
theStorehouse :: LocationCard TheStorehouse
theStorehouse = location (TheStorehouse . (underneathFaceupL .~ True)) Cards.theStorehouse

-- | Forced: After The Storehouse becomes the active location, add the enemy under it to the
-- staging area. That enemy gets +3 Attack and -30 engagement cost until the end of the
-- round.
instance HasAbilities TheStorehouse where
  getAbilities (TheStorehouse a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Forced: When The Storehouse is explored, the first player gains control of the
-- objective ally it was guarding: the Captive goes free as its guard leaves play (see
-- 'runCaptive').
instance RunMessage TheStorehouse where
  runMessage msg l@(TheStorehouse a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ addToStaging (wardensUnder a)
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      wardens <- wardensReleased a
      for_ wardens \e -> roundModifiers a e [AttackModifier 3, EngagementCostModifier (-30)]
      pure $ TheStorehouse $ a {locationUnderneath = []}
    _ -> TheStorehouse <$> runMessage msg a
