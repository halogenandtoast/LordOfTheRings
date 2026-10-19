module LordOfTheRings.Event (module LordOfTheRings.Event, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Event.Cards.CommonCause
import LordOfTheRings.Event.Cards.EverVigilant
import LordOfTheRings.Event.Cards.ForGondor
import LordOfTheRings.Event.Cards.GrimResolve
import LordOfTheRings.Event.Cards.SneakAttack
import LordOfTheRings.Event.Cards.ValiantSacrifice
import LordOfTheRings.Event.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | The event a card is, built to be played; Nothing for events not implemented yet.
lookupEvent :: CardCode -> CardId -> PlayerId -> Maybe Event
lookupEvent cardCode cardId pid = case Map.lookup cardCode allEvents of
  Just (SomeEventCard builder) -> Just $ Event $ cbCardBuilder builder cardId pid
  Nothing -> Nothing

allEvents :: Map CardCode SomeEventCard
allEvents =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeEventCard builder) <-
        [ SomeEventCard everVigilant
        , SomeEventCard commonCause
        , SomeEventCard forGondor
        , SomeEventCard sneakAttack
        , SomeEventCard valiantSacrifice
        , SomeEventCard grimResolve
        ]
    ]
