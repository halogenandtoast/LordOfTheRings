module LordOfTheRings.Scenario.Scenarios.DeadmensDike (deadmensDike) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithTitle), cardMatches, getRemainingPlayers)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype DeadmensDike = DeadmensDike ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadmensDike :: Scenario
deadmensDike =
  Scenario
    $ scenario (DeadmensDike . (nightmareDeckL ?~ nightmare)) "deadmens-dike" "Deadmen's Dike"
 where
  -- ponytail: its "Each player's deck cannot include more than 50 total cards" is left to
  -- the players, decks being built before the game
  nightmare =
    nightmareDeck
      Nightmare.deadmensDikeNightmare
      EncounterSet.DeadmensDikeNightmare
      [ (Locations.fornostSquare, 2)
      , (Locations.brokenBattlements, 3)
      , (Locations.norburyTombs, 2)
      , (Treacheries.restlessEvil, 2)
      ]

-- | Deadmen's Dike, Cursed Dead, Dark Sorcery, Ruins of Arnor and The Lost Realm (Iârion)
-- make up the encounter deck. A player with no cards in his deck is eliminated: see the
-- stages.
-- Nightmare setup: "Each player searches the encounter deck for a copy of Cursed Dead and
-- places it in the encounter discard pile." The copies are alike, so the first ones found go.
instance RunMessage DeadmensDike where
  runMessage msg (DeadmensDike attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.DeadmensDike
          , EncounterSet.CursedDead
          , EncounterSet.DarkSorcery
          , EncounterSet.RuinsOfArnor
          , EncounterSet.TheLostRealm
          ]
      players <- length <$> getRemainingPlayers
      let cursed = if attrs.nightmare then take players (filter (cardMatches $ CardWithTitle "Cursed Dead") cards) else []
          encounterDeck = filter (`notElem` cursed) cards
      firstStage <- genEncounterCard Quests.theShadesOfAngmar
      laterStages <- traverse genEncounterCard [Quests.aFellWraith]
      putQuestIntoPlay firstStage
      pure
        $ DeadmensDike
        $ attrs
        & encounterDeckL .~ encounterDeck
        & encounterDiscardL .~ cursed
        & questDeckL .~ laterStages
    _ -> DeadmensDike <$> runMessage msg attrs
