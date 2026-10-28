module LordOfTheRings.Event.Cards.AWatchfulPeace (aWatchfulPeace) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype AWatchfulPeace = AWatchfulPeace EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a location worth no victory points leaves play as an explored
-- location, return it to the top of the encounter deck.
aWatchfulPeace :: EventCard AWatchfulPeace
aWatchfulPeace = eventWith AWatchfulPeace Cards.aWatchfulPeace (windowL ?~ ExploredWithoutVictoryWindow When)

instance RunMessage AWatchfulPeace where
  runMessage msg e@(AWatchfulPeace attrs) = case msg of
    -- ponytail: played as the location is explored, so it goes to the top of the deck as
    -- it leaves play in place of the discard pile
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [lid | Window _ (Explored lid) <- windows] \lid -> phaseModifier attrs lid LeavesPlayToTopOfEncounterDeck
      pure e
    _ -> AWatchfulPeace <$> runMessage msg attrs
