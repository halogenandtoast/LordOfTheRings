module LordOfTheRings.Location.Cards.PineSlopes (pineSlopes) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PineSlopes = PineSlopes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pineSlopes :: LocationCard PineSlopes
pineSlopes = location PineSlopes Cards.pineSlopes

instance HasAbilities PineSlopes where
  getAbilities (PineSlopes a) = [mkAbility a 1 $ forced $ BagginsResourceSpentOrDiscardedWindow After]

instance RunMessage PineSlopes where
  runMessage msg l@(PineSlopes attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      dealShadowCardsDuringAttack eid pid defenders 2
      pure l
    _ -> PineSlopes <$> runMessage msg attrs
