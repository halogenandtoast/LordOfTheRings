module LordOfTheRings.Location.Cards.Caradhras (caradhras) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype Caradhras = Caradhras LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

caradhras :: LocationCard Caradhras
caradhras = location Caradhras Cards.caradhras

-- | While Caradhras is the active location, questing characters get -1 [Willpower].
-- Players cannot travel to Caradhras except by quest card effects (Snowdrifts makes it
-- the active location without traveling).
instance HasModifiersFor Caradhras where
  getModifiersFor (Caradhras a) = do
    modified a a [CannotBeTraveledTo]
    when (a.placement == ActiveLocation) do
      questing <- selectCharacters CommittedHero CommittedAlly
      for_ questing \c -> modified a c [WillpowerModifier (-1)]
