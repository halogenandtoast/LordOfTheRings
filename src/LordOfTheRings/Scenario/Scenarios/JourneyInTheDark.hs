module LordOfTheRings.Scenario.Scenarios.JourneyInTheDark (journeyInTheDark) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Modifier (ModifierType (HeroesCannotLeaveDiscard, CannotBeDefeated))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (burdenObjectives, burdenTreacheries, earnBurdens)

newtype JourneyInTheDark = JourneyInTheDark ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyInTheDark :: Scenario
journeyInTheDark = Scenario $ scenario (JourneyInTheDark . (nightmareDeckL ?~ nightmare)) "journey-in-the-dark" "Journey in the Dark"
 where
  -- https://hallofbeorn.com/LotR/Details/Journey-in-the-Dark-Nightmare-JitDN
  nightmare = nightmareDeck Nightmare.journeyInTheDarkNightmare EncounterSet.JourneyInTheDarkNightmare
    [(Enemies.moriaArcher, 3), (Locations.minesOfMoria, 4), (Treacheries.weCannotGetOut, 1), (Treacheries.deepFissure, 3)]

instance HasModifiersFor JourneyInTheDark where
  getModifiersFor (JourneyInTheDark a) = when a.nightmare do
    modified a GameTarget [HeroesCannotLeaveDiscard]
    wonBalrog <- getsGame (any (cardMatches $ cardIs Enemies.theBalrog) . (.victoryDisplay) . toAttrs . (.scenario))
    unless wonBalrog do
      stages <- getsGame (map toTarget . filter ((== toCardCode Quests.theBridgeOfKhazadDum) . toCardCode) . toList . (.entities.quests))
      for_ stages (\q -> modified a q [CannotBeDefeated])

{- | The Journey in the Dark encounter set makes up the encounter deck (The Road Darkens
rules insert). Stage 1A sets The Balrog, The Great Bridge and Chamber of Mazarbul aside,
and adds Doom, Doom, Doom to the staging area: all four are kept out of the deck here.

The first player takes control of the Ring-bearer as the game sets up (see
'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage JourneyInTheDark where
  runMessage msg (JourneyInTheDark attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.JourneyInTheDark]
      let aside c = any (`cardMatches` c) (map cardIs [Enemies.theBalrog, Locations.theGreatBridge, Locations.chamberOfMazarbul, Objectives.doomDoomDoom])
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.theLongDarkOfMoria
      laterStages <- traverse genEncounterCard [Quests.drumsInTheDeep, Quests.theBridgeOfKhazadDum]
      putQuestIntoPlay firstStage
      pure
        $ JourneyInTheDark
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    -- Campaign: Setup: Shuffle the burden treachery cards Pursued by the Enemy and Shadow of
    -- Fear into the encounter deck. Add the burden objective cards Grievous Wound and
    -- Overcome by Grief to the staging area. One already in the Campaign Pool is in the
    -- game from the pool (see the scenario runner's setup steps 2 and 4), not twice.
    DoStep 9 Setup | Just l <- attrs.campaign -> do
      let fresh = filter (\def -> toCardCode def `notElem` map (.code) l.pool)
      treacheries <- traverse genEncounterCard (fresh burdenTreacheries)
      traverse_ (addToStaging <=< genEncounterCard) (fresh burdenObjectives)
      deck <- shuffleM (treacheries <> attrs.encounterDeck)
      JourneyInTheDark <$> runMessage msg (attrs & encounterDeckL .~ deck)
    -- Resolution: see 'earnBurdens'.
    ResolveCampaign | isJust attrs.campaign -> do
      earnBurdens
      JourneyInTheDark <$> runMessage msg attrs
    _ -> JourneyInTheDark <$> runMessage msg attrs
