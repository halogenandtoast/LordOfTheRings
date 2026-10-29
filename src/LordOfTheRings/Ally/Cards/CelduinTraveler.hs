module LordOfTheRings.Ally.Cards.CelduinTraveler (celduinTraveler) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (LocationType))

newtype CelduinTraveler = CelduinTraveler AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Secrecy 2, on its card def.
celduinTraveler :: AllyCard CelduinTraveler
celduinTraveler = ally CelduinTraveler Cards.celduinTraveler

-- | Response: After this ally enters play, look at the top card of the encounter deck. If
-- it is a location, you may discard it.
instance HasAbilities CelduinTraveler where
  getAbilities (CelduinTraveler a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage CelduinTraveler where
  runMessage msg s@(CelduinTraveler a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lookAtTopOfEncounterDeckMayDiscard pid a LocationType
      pure s
    _ -> CelduinTraveler <$> runMessage msg a
