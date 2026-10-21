module LordOfTheRings.Event.Cards.StridersPath (stridersPath) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype StridersPath = StridersPath EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a location is revealed from the encounter deck, immediately travel to
-- that location without resolving its Travel effect. If another location is currently
-- active, return it to the staging area.
stridersPath :: EventCard StridersPath
stridersPath = eventWith StridersPath Cards.stridersPath (windowL ?~ LocationRevealedWindow After)

instance RunMessage StridersPath where
  runMessage msg e@(StridersPath attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [lid | Window _ (Revealed (LocationTarget lid)) <- windows] \lid -> do
        active <- select IsActiveLocation
        for_ active \old -> placeLocation old StagingArea
        travelWithoutCost lid
      pure e
    _ -> StridersPath <$> runMessage msg attrs
