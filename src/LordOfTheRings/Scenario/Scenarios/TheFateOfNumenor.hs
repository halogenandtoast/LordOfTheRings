module LordOfTheRings.Scenario.Scenarios.TheFateOfNumenor (theFateOfNumenor) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (LocationMatcher (LocationInStagingArea), getLocation, select, unchartedLocation)
import LordOfTheRings.Modifier (ModifierType (ProgressLimit))
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheFateOfNumenor = TheFateOfNumenor ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFateOfNumenor :: Scenario
theFateOfNumenor =
  Scenario
    $ scenario (TheFateOfNumenor . (nightmareDeckL ?~ nightmare)) "the-fate-of-numenor" "The Fate of Númenor"
 where
  -- the Nightmare Deck reprints the Uncharted deck: every standard Uncharted location
  -- comes out, its own come in
  nightmare =
    nightmareDeck
      Nightmare.theFateOfNumenorNightmare
      EncounterSet.TheFateOfNumenorNightmare
      [ (Locations.forbiddenCoast, 2)
      , (Locations.cursedTemple, 2)
      , (Locations.floodedRuins, 2)
      , (Locations.drownedGraves, 2)
      , (Locations.lushJungle, 2)
      , (Locations.shrineToMorgoth, 1)
      , (Locations.steepPlateau, 3)
      , (Treacheries.aimlessWandering, 3)
      , (Treacheries.mysteriousFog, 1)
      , (Enemies.servantOfTheDeceiver, 1)
      , (Treacheries.lingeringMalevolence, 1)
      , (Enemies.drownedDead, 1)
      ]

-- | Nightmare: "Progress cannot be placed on non-Uncharted locations in the staging area."
instance HasModifiersFor TheFateOfNumenor where
  getModifiersFor (TheFateOfNumenor a) = when a.nightmare do
    charted <- select (LocationInStagingArea <> not_ unchartedLocation)
    for_ charted \l -> modified a l [ProgressLimit 0]

{- | The encounter deck is built from The Fate of Númenor, Ruins of Numenor and Drowned
Dead. Their Uncharted locations, under a Lost Island side, are the Uncharted deck: Shrine
to Morgoth is set aside, and the rest, shuffled, are the Uncharted deck (The Grey Havens
rules insert). Any other location, as the Nightmare Deck's Overgrown Steps, stays in the
encounter deck.
-}
instance RunMessage TheFateOfNumenor where
  runMessage msg (TheFateOfNumenor attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheFateOfNumenor, EncounterSet.RuinsOfNumenor, EncounterSet.DrownedDead]
      let isUncharted = (== Just (toCardCode Locations.lostIsland)) . cdOtherSide . toCardDef
          (locations, encounterDeck) = partition isUncharted cards
          (shrine, uncharted) = partition ((`elem` map toCardCode Locations.fateOfNumenorShrines) . toCardCode) locations
      unchartedDeck <- shuffleM uncharted
      firstStage <- genEncounterCard Quests.exploreTheIsland
      laterStages <- traverse genEncounterCard [Quests.aFatefulDiscovery]
      putQuestIntoPlay firstStage
      pure
        $ TheFateOfNumenor
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ shrine
        & unchartedDeckL .~ unchartedDeck
    -- Nightmare: "Forced: When the players would travel to Shrine to Morgoth, if there are
    -- 3 or fewer double-sided locations in the victory display, return Shrine to Morgoth to
    -- the staging area and flip it to its Lost Island side. Remove all progress from each
    -- Uncharted location in the staging area and shuffle them so that the players do not
    -- know which one is which. Then, choose a new location to travel to."
    TravelTo lid | attrs.nightmare -> do
      l <- getLocation lid
      let doubleSided = length [() | EncounterCard c <- attrs.victoryDisplay, isJust (cdOtherSide $ toCardDef c)]
      when (toCardCode l `elem` map toCardCode Locations.fateOfNumenorShrines && doubleSided <= 3) do
        cancelMatching \case
          DoStep 1 (TravelTo l') -> l' == lid
          _ -> False
        flipOver lid
        shuffleUnchartedLocations
        doStep 1 (BeginPhase TravelPhase)
      TheFateOfNumenor <$> runMessage msg attrs
    _ -> TheFateOfNumenor <$> runMessage msg attrs
