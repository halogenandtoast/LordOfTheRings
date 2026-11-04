module LordOfTheRings.Location.Cards.MountainPass (mountainPass) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Weather))

newtype MountainPass = MountainPass LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainPass :: LocationCard MountainPass
mountainPass = location MountainPass Cards.mountainPass

-- | While Mountain Pass is in the staging area, the 'when revealed' effects of Weather
-- treachery cards cannot be canceled.
instance HasModifiersFor MountainPass where
  getModifiersFor (MountainPass a) = when (a.placement == StagingArea) do
    weather <- getsGame (filter ((Weather `elem`) . cdCardTraits . toCardDef) . toList . (.treacheries) . gameEntities)
    for_ weather \t -> modified a t [WhenRevealedCannotBeCanceled]

instance RunMessage MountainPass where
  runMessage msg (MountainPass a) = MountainPass <$> runMessage msg a
