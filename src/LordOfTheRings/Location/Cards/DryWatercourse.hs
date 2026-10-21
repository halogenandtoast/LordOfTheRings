module LordOfTheRings.Location.Cards.DryWatercourse (dryWatercourse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DryWatercourse = DryWatercourse LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | While Dry Watercourse is the active location, all treachery card effects that target
-- the player guarding Gollum also target each other player. (The Tantrum treacheries ask
-- the scenario's helpers whom they reach.)
dryWatercourse :: LocationCard DryWatercourse
dryWatercourse = location DryWatercourse Cards.dryWatercourse
