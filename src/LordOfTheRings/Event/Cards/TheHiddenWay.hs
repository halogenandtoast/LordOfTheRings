module LordOfTheRings.Event.Cards.TheHiddenWay (theHiddenWay) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype TheHiddenWay = TheHiddenWay EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: At the beginning of the quest phase, search the top 5 cards of the encounter
-- deck for a location and make it the active location, returning any previous active
-- location to the staging area. Then, reveal 1 less encounter card this phase (to a
-- minimum of 0). Shuffle the encounter deck.
theHiddenWay :: EventCard TheHiddenWay
theHiddenWay = eventWith TheHiddenWay Cards.theHiddenWay (windowL ?~ PhaseBeginsWindow After QuestPhase)

instance RunMessage TheHiddenWay where
  runMessage msg e@(TheHiddenWay attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- getsGame (take 5 . (.encounterDeck) . toAttrs . (.scenario))
      active <- select IsActiveLocation
      let locations = filter (cardMatches $ CardWithType LocationType) top
      chooseOneM pid do
        questionLabeled "theHiddenWay.location"
        causedBy attrs
        showing (map EncounterCard top)
        targets locations \c -> do
          removeFromEncounterDeck c
          for_ active \lid -> placeLocation lid StagingArea
          putIntoPlayAsActiveLocation c
        when (null locations) $ labeled "theHiddenWay.none" (pure ())
      phaseModifier attrs GameTarget (FewerStagingReveals 1)
      shuffleEncounterDeck
      pure e
    _ -> TheHiddenWay <$> runMessage msg attrs
