module LordOfTheRings.Game.SearchZones (searchZones) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Game.Base
import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Question (searchedCards)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

-- | Where each card a question searches lies, so the search shows its cards by zone: a
-- player's deck, hand or discard pile, by player, or one of the scenario's. A card in
-- none of these, as one revealed or set aside by an effect, is left out.
searchZones :: Game -> Map CardId Value
searchZones g = mapFromList [(c.id, z) | (z, cs) <- zones, c <- cs, c.id `member` searched]
 where
  searched = setFromList [c.id | q <- toList g.question, c <- searchedCards q] :: Set CardId
  a = toAttrs g.scenario
  encounter = map EncounterCard
  zones =
    [ (object ["zone" .= zone, "player" .= p.id], cs)
    | p <- toList (allPlayers g)
    , (zone, cs) <- [("deck" :: Text, p.deck), ("hand", p.hand), ("discard", p.discard)]
    ]
      <> [ (object ["zone" .= ("encounterDeck" :: Text)], encounter $ scenarioEncounterDeck a)
         , (object ["zone" .= ("encounterDiscard" :: Text)], encounter $ scenarioEncounterDiscard a)
         , (object ["zone" .= ("corsairDeck" :: Text)], encounter $ fromMaybe [] $ scenarioCorsairDeck a)
         , (object ["zone" .= ("corsairDiscard" :: Text)], encounter $ scenarioCorsairDiscard a)
         , (object ["zone" .= ("victory" :: Text)], scenarioVictoryDisplay a)
         ]
