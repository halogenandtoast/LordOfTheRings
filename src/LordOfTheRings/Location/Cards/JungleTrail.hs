module LordOfTheRings.Location.Cards.JungleTrail (jungleTrail) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype JungleTrail = JungleTrail LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

jungleTrail :: LocationCard JungleTrail
jungleTrail = location JungleTrail Cards.jungleTrail

-- | While Jungle Trail is the active location, each location in the staging area gets -1
-- threat.
instance HasModifiersFor JungleTrail where
  getModifiersFor (JungleTrail a) = when (a.placement == ActiveLocation) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ThreatModifier (-1)]

-- | Travel: Reveal the top card of the encounter deck to travel here.
instance RunMessage JungleTrail where
  runMessage msg l@(JungleTrail a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      pure l
    _ -> JungleTrail <$> runMessage msg a
