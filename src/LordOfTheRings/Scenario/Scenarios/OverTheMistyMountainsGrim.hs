module LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim (overTheMistyMountainsGrim) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted

newtype OverTheMistyMountainsGrim = OverTheMistyMountainsGrim ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overTheMistyMountainsGrim :: Scenario
overTheMistyMountainsGrim =
  Scenario
    $ scenario
      (OverTheMistyMountainsGrim . (nightmareDeckL ?~ nightmare))
      "over-the-misty-mountains-grim"
      "Over the Misty Mountains Grim"
 where
  -- The Nightmare setup card removes cards from both decks before they are separated.
  -- https://hallofbeorn.com/LotR/Details/Over-the-Misty-Mountains-Grim-Nightmare-OtMMGN
  nightmare =
    nightmareDeck
      Nightmare.overTheMistyMountainsGrimNightmare
      EncounterSet.OverTheMistyMountainsGrimNightmare
      [ (Locations.caveEntranceWesternLands, 2)
      , (Locations.loneLands, 2)
      , (Locations.drearyHills, 2)
      , (Locations.overhangingRock, 2)
      , (Locations.greatCavernRoom, 1)
      , (Enemies.theGreatGoblin, 1)
      , (Enemies.goblinMiners, 3)
      , (Enemies.goblinAxeman, 3)
      ]

-- | The cards of the second encounter deck: The Great Goblin and Misty Mountain Goblins sets.
inSecondDeck :: EncounterCard -> Bool
inSecondDeck =
  ( `elem`
      map
        Just
        [ EncounterSet.TheGreatGoblin
        , EncounterSet.MistyMountainGoblins
        , EncounterSet.TheGreatGoblinNightmare
        ]
  )
    . cdEncounterSet
    . toCardDef

{- | Played with two encounter decks (rules insert, "Active and Inactive Encounter Decks"):
the Over the Misty Mountains Grim and Western Lands sets make up the active encounter
deck, The Great Goblin and Misty Mountain Goblins sets the second, set aside inactive
until Down, Down to Goblin Town makes it the active one. Stage 1A's setup is this.
-}
instance RunMessage OverTheMistyMountainsGrim where
  runMessage msg (OverTheMistyMountainsGrim attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          $ [ EncounterSet.OverTheMistyMountainsGrim
            , EncounterSet.WesternLands
            , EncounterSet.TheGreatGoblin
            , EncounterSet.MistyMountainGoblins
            ]
          <> [EncounterSet.TheGreatGoblinNightmare | attrs.nightmare]
      let (secondDeck, encounterDeck) = partition inSecondDeck cards
      firstStage <- genEncounterCard Quests.aShortRest
      laterStages <- traverse genEncounterCard [Quests.theMountainPass, Quests.downDownToGoblinTown]
      putQuestIntoPlay firstStage
      pure
        $ OverTheMistyMountainsGrim
        $ attrs
        & encounterDeckL
        .~ encounterDeck
        & questDeckL
        .~ laterStages
        & setAsideL
        .~ secondDeck
    -- The first deck, the encounter cards in play shuffled into it, and its discard pile
    -- are set aside inactive. Nothing makes them active again, so they are let go rather
    -- than kept with the set-aside cards, where effects reading set-aside cards would find
    -- them.
    ScenarioSpecific "overTheMistyMountainsGrim.switchEncounterDeck" _ -> do
      deck <- shuffleM attrs.scenarioSetAside
      pure
        $ OverTheMistyMountainsGrim
        $ attrs
        & encounterDeckL
        .~ deck
        & encounterDiscardL
        .~ []
        & setAsideL
        .~ []
    _ -> OverTheMistyMountainsGrim <$> runMessage msg attrs
