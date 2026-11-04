module LordOfTheRings.Scenario.Scenarios.MountGundabad (mountGundabad) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Id (CardId)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers (returnDagnirToStaging)
import LordOfTheRings.Window (WindowType (AdvancedToStage), mkAfter)

newtype MountGundabad = MountGundabad ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountGundabad :: Scenario
mountGundabad = Scenario $ scenario MountGundabad "mount-gundabad" "Mount Gundabad"

{- | Mount Gundabad, Fell Beasts, Deep Underground, Afraid of the Dark, Lost Caves and Dragon
Might make up the encounter deck (MEC70 rules). Lost Caves is the Caves deck, and so are
Throat of the Mountain and Dagnir's Hoard: Caleb ruled that they are "supposed to be part
of the Caves deck" (FFG forum, "Mount Gundabad Caves Deck", May 2019). Dagnir, The First
Forge and Wormsbane are set aside for stage 1A; Wormsbane's Reforged side is no card of its
own. The seven stage 2 cards are shuffled into the quest deck, side 2A faceup.
-}
instance RunMessage MountGundabad where
  runMessage msg (MountGundabad attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.MountGundabad
          , EncounterSet.FellBeasts
          , EncounterSet.DeepUnderground
          , EncounterSet.AfraidOfTheDark
          , EncounterSet.LostCaves
          , EncounterSet.DragonMight
          ]
      let inCaves c =
            cdEncounterSet (toCardDef c) == Just EncounterSet.LostCaves
              || cardMatches (CardMatchAny [cardIs Locations.throatOfTheMountain, cardIs Locations.dagnirsHoard]) c
          (caves, rest) = partition inCaves cards
          (aside, deck) =
            partition (cardMatches $ CardMatchAny [cardIs Enemies.dagnirMountGundabad, cardIs Locations.theFirstForge, cardIs Objectives.wormsbane])
              $ filter (not . cardMatches (cardIs Objectives.wormsbaneReforged)) rest
      cavesDeck <- shuffleM caves
      firstStage <- genEncounterCard Quests.theSideDoor
      laterStages <-
        shuffleM
          =<< traverse
            genEncounterCard
            [ Quests.theUpperLevels
            , Quests.theLowerDeeps
            , Quests.frenziedWarrens
            , Quests.dagnirsWrath
            , Quests.forebodingDarkness
            , Quests.imminentPursuit
            , Quests.theGreatWorm
            ]
      putQuestIntoPlay firstStage
      pure
        $ MountGundabad
        $ attrs
        & encounterDeckL .~ deck
        & cavesDeckL .~ cavesDeck
        & setAsideL .~ aside
        & questDeckL .~ laterStages
    -- the top card of the quest deck turned over in the deck, as Foreboding Darkness does,
    -- comes out on its B side, its side 2A not revealed
    Flip (CardIdTarget cid) | any ((== cid) . (.id)) attrs.questDeck -> pure $ MountGundabad $ attrs & metaL .~ toJSON (Just cid)
    AdvanceToQuest card | flippedInDeck == Just card.id -> do
      putQuestIntoPlayFlipped card
      push $ CheckWindows [mkAfter AdvancedToStage]
      pure $ MountGundabad $ attrs & metaL .~ Null
    -- a stage 2 is revealed side 2A faceup, and flips itself at the beginning of the quest
    -- phase. Each side 2A reads "When Revealed: Return Dagnir to the staging area."
    AdvanceToQuest card -> do
      putQuestIntoPlay card
      returnDagnirToStaging
      push $ CheckWindows [mkAfter AdvancedToStage]
      pure $ MountGundabad attrs
    _ -> MountGundabad <$> runMessage msg attrs
   where
    flippedInDeck = join (parseMaybe parseJSON attrs.meta) :: Maybe CardId
