module LordOfTheRings.Location.Cards.DarkTunnel (darkTunnel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dark))

newtype DarkTunnel = DarkTunnel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkTunnel :: LocationCard DarkTunnel
darkTunnel = location DarkTunnel Cards.darkTunnel

-- | Deep. While Dark Tunnel is in the staging area, progress cannot be placed on Dark
-- locations in the staging area.
instance HasModifiersFor DarkTunnel where
  getModifiersFor (DarkTunnel a) = when (a.placement == StagingArea) do
    dark <- select (LocationInStagingArea <> LocationWithTrait Dark)
    for_ dark \l -> modified a l [ProgressLimit 0]

instance RunMessage DarkTunnel where
  runMessage msg (DarkTunnel a) = DarkTunnel <$> runMessage msg a
