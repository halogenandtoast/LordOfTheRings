module LordOfTheRings.Player.Runner (playableCardsByEffect, playableCardsFromDeckByEffect, collectionEventChoices) where

import LordOfTheRings.Card (Card, CardDef)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher.Types (CardMatcher)
import LordOfTheRings.Prelude

playableCardsByEffect :: HasGame m => PlayerId -> CardMatcher -> Int -> m [Card]
playableCardsFromDeckByEffect :: HasGame m => PlayerId -> Int -> m [Card]
collectionEventChoices :: HasGame m => PlayerId -> m [CardDef]
