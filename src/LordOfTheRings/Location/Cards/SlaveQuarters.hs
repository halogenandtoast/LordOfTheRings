module LordOfTheRings.Location.Cards.SlaveQuarters (slaveQuarters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (wardensReleased, wardensUnder)

newtype SlaveQuarters = SlaveQuarters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Warden under it lies faceup.
slaveQuarters :: LocationCard SlaveQuarters
slaveQuarters = location (SlaveQuarters . (underneathFaceupL .~ True)) Cards.slaveQuarters

-- | Forced: After Slave Quarters becomes the active location, the enemy under it engages
-- the first player and makes an immediate attack.
instance HasAbilities SlaveQuarters where
  getAbilities (SlaveQuarters a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Forced: When Slave Quarters is explored, the first player gains control of the
-- objective ally it was guarding: the Captive goes free as its guard leaves play (see
-- 'runCaptive').
instance RunMessage SlaveQuarters where
  runMessage msg l@(SlaveQuarters a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      traverse_ (putEnemyIntoPlayEngaged lead) (wardensUnder a)
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      lead <- getLead
      wardens <- wardensReleased a
      for_ wardens (immediateAttack lead)
      pure $ SlaveQuarters $ a {locationUnderneath = []}
    _ -> SlaveQuarters <$> runMessage msg a
