module LordOfTheRings.Ally.Cards.WoodlandCourier (woodlandCourier) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Forest))

newtype WoodlandCourier = WoodlandCourier AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodlandCourier :: AllyCard WoodlandCourier
woodlandCourier = ally WoodlandCourier Cards.woodlandCourier

-- | Response: After Woodland Courier enters play, place 1 progress on a location (2
-- progress instead if that location has the Forest trait).
instance HasAbilities WoodlandCourier where
  getAbilities (WoodlandCourier a) = [restricted a 1 (exists AnyLocation) $ response $ entersPlay a]

instance RunMessage WoodlandCourier where
  runMessage msg s@(WoodlandCourier a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "woodlandCourier.location"
        causedBy a
        targets locations \l -> do
          forest <- selectAny (LocationWithId l <> LocationWithTrait Forest)
          placeProgress a l (if forest then 2 else 1)
      pure s
    _ -> WoodlandCourier <$> runMessage msg a
