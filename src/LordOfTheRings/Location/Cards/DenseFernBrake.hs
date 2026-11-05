module LordOfTheRings.Location.Cards.DenseFernBrake (denseFernBrake) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype DenseFernBrake = DenseFernBrake LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

denseFernBrake :: LocationCard DenseFernBrake
denseFernBrake = location DenseFernBrake Cards.denseFernBrake

instance HasModifiersFor DenseFernBrake where
  getModifiersFor (DenseFernBrake a) = when (a.placement == ActiveLocation) do
    enemies <- select EnemyInStagingArea
    for_ enemies \eid -> modified a eid [ImmuneToPlayerCardEffects]

instance RunMessage DenseFernBrake where
  runMessage msg l@(DenseFernBrake a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseAndExhaust a pid 1
      pure l
    _ -> DenseFernBrake <$> runMessage msg a
