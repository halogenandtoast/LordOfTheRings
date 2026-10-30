module LordOfTheRings.Quest.Cards.ScatteredAmongTheHills (scatteredAmongTheHills) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (orcAmbush, revealFromOrcDeck)
import LordOfTheRings.Trait (Trait (Weather))

newtype ScatteredAmongTheHills = ScatteredAmongTheHills QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

scatteredAmongTheHills :: QuestCard ScatteredAmongTheHills
scatteredAmongTheHills = quest ScatteredAmongTheHills Cards.scatteredAmongTheHills

-- | The first Weather treachery revealed each round gains surge.
-- Forced: After the active location is explored, reveal the top card of the Orc deck and
-- resolve its staging as if it was just revealed from the encounter deck (the top 2 cards
-- instead if there are 3 or more players in the game).
-- The surge is an encounter card revealed once the treachery has resolved; the Forced
-- effect answers as the active location is explored, while it is still the active one.
instance HasAbilities ScatteredAmongTheHills where
  getAbilities (ScatteredAmongTheHills a) =
    [ ability
    | a.flipped
    , ability <-
        [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Weather
        , mkAbility a 2 $ forced $ ActiveLocationExploredWindow When
        ]
    ]

-- | Setup: Set Orc Ambush and Amon Forn aside, out of play. Create the Orc deck (see
-- insert) and set it next to the quest deck. Make Exposed Ridge the active location. Add
-- Hunting the Orcs and 1 Weathered Hilltop to the staging area. Shuffle the encounter deck.
-- The scenario made the Orc deck as it gathered the encounter deck.
instance RunMessage ScatteredAmongTheHills where
  runMessage msg q@(ScatteredAmongTheHills attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies def = filter ((== toCardCode def) . toCardCode) deck
          firstOf def f = for_ (take 1 $ copies def) \card -> do
            removeFromEncounterDeck card
            f card
      traverse_ setAside $ copies Locations.amonForn <> filter (cardMatches orcAmbush) deck
      firstOf Locations.exposedRidge putIntoPlayAsActiveLocation
      firstOf Objectives.huntingTheOrcs addToStaging
      firstOf Locations.weatheredHilltop addToStaging
      shuffleEncounterDeck
      pure $ ScatteredAmongTheHills $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      players <- length <$> getRemainingPlayers
      revealFromOrcDeck (if players >= 3 then 2 else 1)
      pure q
    _ -> ScatteredAmongTheHills <$> runMessage msg attrs
