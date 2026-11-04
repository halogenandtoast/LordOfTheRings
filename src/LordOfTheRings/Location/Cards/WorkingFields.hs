module LordOfTheRings.Location.Cards.WorkingFields (workingFields) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype WorkingFields = WorkingFields LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

workingFields :: LocationCard WorkingFields
workingFields = location WorkingFields Cards.workingFields

-- | While Working Fields is in the staging area, characters cannot be healed.
instance HasModifiersFor WorkingFields where
  getModifiersFor (WorkingFields a) = when (a.placement == StagingArea) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [CannotBeHealed]

-- | Travel: Each player deals 1 damage to a character he controls.
instance RunMessage WorkingFields where
  runMessage msg (WorkingFields a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> dealDamageToOneCharacter a pid 1
      WorkingFields <$> runMessage msg a
    _ -> WorkingFields <$> runMessage msg a
