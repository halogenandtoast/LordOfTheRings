-- | What the cards of The King's Quest share.
module LordOfTheRings.Scenario.Scenarios.TheKingsQuest.Helpers (locationsInVictoryDisplay) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType), EncounterCard)
import LordOfTheRings.Classes.Entity (Entity (toAttrs))
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Matcher (CardMatcher (CardWithType), cardMatches)

-- | The locations in the victory display, as Contested Depths and stage 1B count them.
locationsInVictoryDisplay :: HasGame m => m [EncounterCard]
locationsInVictoryDisplay =
  getsGame \g -> [c | EncounterCard c <- (toAttrs g.scenario).victoryDisplay, cardMatches (CardWithType LocationType) c]
