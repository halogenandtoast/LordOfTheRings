module LordOfTheRings.Location.Cards.DeepChasm (deepChasm) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DeepChasm = DeepChasm LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepChasm :: LocationCard DeepChasm
deepChasm = location DeepChasm Cards.deepChasm

-- | Deep. When faced with the option to travel, the players must travel to Deep Chasm if
-- there is no active location.
instance HasModifiersFor DeepChasm where
  getModifiersFor (DeepChasm a) = do
    active <- selectAny IsActiveLocation
    unless active $ modifySelf a [MustBeTraveledTo]
