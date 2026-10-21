module LordOfTheRings.Event (module LordOfTheRings.Event, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Event.Cards.CommonCause
import LordOfTheRings.Event.Cards.GandalfsSearch
import LordOfTheRings.Event.Cards.WillOfTheWest
import LordOfTheRings.Event.Cards.DwarvenTomb
import LordOfTheRings.Event.Cards.FortuneOrFate
import LordOfTheRings.Event.Cards.StandAndFight
import LordOfTheRings.Event.Cards.ATestOfWill
import LordOfTheRings.Event.Cards.HastyStroke
import LordOfTheRings.Event.Cards.RainOfArrows
import LordOfTheRings.Event.Cards.QuickStrike
import LordOfTheRings.Event.Cards.StandTogether
import LordOfTheRings.Event.Cards.ThicketOfSpears
import LordOfTheRings.Event.Cards.Feint
import LordOfTheRings.Event.Cards.SwiftStrike
import LordOfTheRings.Event.Cards.StrengthOfWill
import LordOfTheRings.Event.Cards.SecretPaths
import LordOfTheRings.Event.Cards.RadagastsCunning
import LordOfTheRings.Event.Cards.ALightInTheDark
import LordOfTheRings.Event.Cards.TheGaladhrimsGreeting
import LordOfTheRings.Event.Cards.BeornsHospitality
import LordOfTheRings.Event.Cards.LoriensWealth
import LordOfTheRings.Event.Cards.LoreOfImladris
import LordOfTheRings.Event.Cards.BladeMastery
import LordOfTheRings.Event.Cards.EverVigilant
import LordOfTheRings.Event.Cards.ForGondor
import LordOfTheRings.Event.Cards.GrimResolve
import LordOfTheRings.Event.Cards.SneakAttack
import LordOfTheRings.Event.Cards.ValiantSacrifice
import LordOfTheRings.Event.Cards.CampfireTales
import LordOfTheRings.Event.Cards.TheEaglesAreComing
import LordOfTheRings.Event.Cards.MusteringTheRohirrim
import LordOfTheRings.Event.Cards.StridersPath
import LordOfTheRings.Event.Cards.SecondBreakfast
import LordOfTheRings.Event.Cards.PartingGifts
import LordOfTheRings.Event.Cards.ToTheEyrie
import LordOfTheRings.Event.Cards.Infighting
import LordOfTheRings.Event.Cards.RearGuard
import LordOfTheRings.Event.Cards.MeneldorsFlight
import LordOfTheRings.Event.Cards.RideToRuin
import LordOfTheRings.Event.Cards.GildorsCounsel
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
        , SomeEventCard gandalfsSearch
        , SomeEventCard willOfTheWest
        , SomeEventCard dwarvenTomb
        , SomeEventCard fortuneOrFate
        , SomeEventCard standAndFight
        , SomeEventCard aTestOfWill
        , SomeEventCard hastyStroke
        , SomeEventCard rainOfArrows
        , SomeEventCard quickStrike
        , SomeEventCard standTogether
        , SomeEventCard thicketOfSpears
        , SomeEventCard feint
        , SomeEventCard swiftStrike
        , SomeEventCard strengthOfWill
        , SomeEventCard secretPaths
        , SomeEventCard radagastsCunning
        , SomeEventCard aLightInTheDark
        , SomeEventCard theGaladhrimsGreeting
        , SomeEventCard beornsHospitality
        , SomeEventCard loriensWealth
        , SomeEventCard loreOfImladris
        , SomeEventCard bladeMastery
        , SomeEventCard campfireTales
        , SomeEventCard theEaglesAreComing
        , SomeEventCard musteringTheRohirrim
        , SomeEventCard stridersPath
        , SomeEventCard secondBreakfast
        , SomeEventCard partingGifts
        , SomeEventCard toTheEyrie
        , SomeEventCard infighting
        , SomeEventCard rearGuard
        , SomeEventCard meneldorsFlight
        , SomeEventCard rideToRuin
        , SomeEventCard gildorsCounsel
        ]
    ]
