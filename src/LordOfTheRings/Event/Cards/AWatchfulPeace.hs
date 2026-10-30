module LordOfTheRings.Event.Cards.AWatchfulPeace (aWatchfulPeace) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype AWatchfulPeace = AWatchfulPeace EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a location worth no victory points leaves play as an explored
-- location, return it to the top of the encounter deck. (FAQ 1.9 errata; as printed, any
-- location.)
aWatchfulPeace :: EventCard AWatchfulPeace
aWatchfulPeace =
  eventWith AWatchfulPeace Cards.aWatchfulPeace \a ->
    a & windowL ?~ ErrataWindow a.owner (ExploredWithoutVictoryWindow When) (AnyExploredWindow When)

instance RunMessage AWatchfulPeace where
  runMessage msg e@(AWatchfulPeace attrs) = case msg of
    -- played as the location is explored, the effect waits on its card and returns it to
    -- the top of the deck once it has left play, where its immunity no longer applies (FAQ)
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [lid | Window _ (Explored lid) <- windows] \lid -> do
        location <- getLocation lid
        phaseModifier attrs (CardIdTarget (toAttrs location).cardId) LeavesPlayToTopOfEncounterDeck
      pure e
    _ -> AWatchfulPeace <$> runMessage msg attrs
