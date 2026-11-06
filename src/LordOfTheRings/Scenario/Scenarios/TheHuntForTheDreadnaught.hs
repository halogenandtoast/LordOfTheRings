module LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught (theHuntForTheDreadnaught) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.HasGame (getGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (
  EpicEffect (EpicDamage),
  EpicTargetQuery (EpicEngagedNonUniqueEnemy),
 )
import LordOfTheRings.Helpers.Modifiers (isEncounterCardSource)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as SetupCards
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source
import LordOfTheRings.Trait (Trait (Basic, Ship))
import LordOfTheRings.Window

newtype TheHuntForTheDreadnaught = TheHuntForTheDreadnaught ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

theHuntForTheDreadnaught :: Scenario
theHuntForTheDreadnaught =
  Scenario
    $ scenario TheHuntForTheDreadnaught "the-hunt-for-the-dreadnaught" "The Hunt for the Dreadnaught"

setupCard :: ScenarioAttrs -> CardDef
setupCard a = case a.mode of
  EasyMode -> SetupCards.dreadnaughtEasySetup
  HardMode -> SetupCards.dreadnaughtHardSetup
  _ -> SetupCards.dreadnaughtStandardSetup

instance HasModifiersFor TheHuntForTheDreadnaught where
  getModifiersFor (TheHuntForTheDreadnaught a) = do
    players <- getRemainingPlayers
    for_ players \pid -> case a.mode of
      EasyMode -> modified a (PlayerTarget pid) [AdditionalResourcePhaseDraws 1]
      HardMode -> modified a (PlayerTarget pid) [ThreatReductionPerRoundLimit 1]
      _ -> pure ()

instance HasAbilities TheHuntForTheDreadnaught where
  getAbilities (TheHuntForTheDreadnaught a) =
    [ anyPlayer
        $ limited (GroupLimit PerGame 1)
        $ restricted a 1 (DuringPhase QuestPhase <> EpicTargetsExist EpicEngagedNonUniqueEnemy)
        $ ActionAbility Free
    | a.mode /= HardMode
    , a.meta == Bool True
    ]

instance RunMessage TheHuntForTheDreadnaught where
  runMessage msg s@(TheHuntForTheDreadnaught attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheHuntForTheDreadnaught]
      let aside card =
            let def = toCardDef card
             in (cdCardType def == EnemyType && cdUnique def) || cdCardType def == ObjectiveAllyType
          (reservedCards, mainDeck) = partition aside cards
          remove def n sourceCards =
            let (copies, rest) = partition ((== toCardCode def) . toCardCode) sourceCards
             in (take n copies, drop n copies <> rest)
          (blackSails, withoutSails) = if attrs.easyMode then remove Enemies.blackSailsDreadnaught 2 mainDeck else ([], mainDeck)
          (warship, deck) =
            if attrs.easyMode
              then remove Enemies.corsairWarshipDreadnaught 1 withoutSails
              else ([], withoutSails)
      firstStage <- genEncounterCard Quests.searchingTheHighSeas
      later <- genEncounterCard Quests.destroyTheDreadnaught
      putQuestIntoPlay firstStage
      push PrepareCorsairDeck
      pure
        $ TheHuntForTheDreadnaught
        $ attrs
        & encounterDeckL
        .~ deck
        & setAsideL
        .~ (reservedCards <> blackSails <> warship)
        & questDeckL
        .~ [later]
        & metaL
        .~ Bool False
    DoStep 4 Setup -> do
      players <- getPlayerOrder
      let ships = filter (cardMatches $ CardWithTrait Ship <> CardWithTrait Basic) attrs.scenarioSetAside
      push $ ChooseFleetShip players ships
      TheHuntForTheDreadnaught <$> runMessage msg attrs
    ChooseFleetShip [] _ -> pure $ TheHuntForTheDreadnaught $ attrs & metaL .~ Bool True
    DoStep 6 Setup -> do
      players <- length <$> getPlayerOrder
      case attrs.mode of
        EasyMode -> do
          let chosen =
                take 1 $ filter ((== toCardCode Locations.cobasHavenDreadnaught) . toCardCode) attrs.encounterDeck
          for_ chosen addToStaging
          pure $ TheHuntForTheDreadnaught $ attrs & encounterDeckL %~ filter (`notElem` chosen)
        HardMode -> do
          let chosen =
                take 3 (filter ((== toCardCode Locations.tolfalasLanding) . toCardCode) attrs.encounterDeck)
                  <> [ card
                     | players >= 3
                     , card <- take 2 $ filter ((== toCardCode Locations.wickedCove) . toCardCode) attrs.encounterDeck
                     ]
          for_ chosen addToStaging
          pure $ TheHuntForTheDreadnaught $ attrs & encounterDeckL %~ filter (`notElem` chosen)
        _ -> do
          let (locations, discarded, rest) = startingLocations (if players >= 3 then 3 else 2) attrs.encounterDeck
          for_ locations addToStaging
          pure
            $ TheHuntForTheDreadnaught
            $ attrs
            & encounterDeckL
            .~ rest
            & encounterDiscardL
            %~ (discarded <>)
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseEpicTarget (setupCard attrs) pid EpicEngagedNonUniqueEnemy (EpicDamage 10)
      when (attrs.mode == NormalMode) $ doStep 1 msg
      pure s
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> revealEncounterCard >> pure s
    CheckWindows windows -> do
      when (attrs.meta == Bool True && any shipLeft windows) checkFleet
      when (attrs.mode == HardMode) do
        g <- getGame
        for_ windows \case
          Window After (Healed target source)
            | isPlayerCardSource source && not (isEncounterCardSource g source) -> penalize target
          Window After (ReadiedByPlayerCard target) -> penalize target
          _ -> pure ()
      TheHuntForTheDreadnaught <$> runMessage msg attrs
    CheckEliminations | attrs.meta == Bool True -> do
      checkFleet
      TheHuntForTheDreadnaught <$> runMessage msg attrs
    _ -> TheHuntForTheDreadnaught <$> runMessage msg attrs
   where
    shipLeft = \case
      Window After (LeftPlay _ _ traits) -> Ship `member` traits
      _ -> False
    checkFleet = eachPlayer \pid -> do
      ships <- traverse getAlly =<< select (AllyControlledBy $ PlayerWithId pid)
      unless
        ( any
            (\a -> cdCardType (toCardDef a) == ObjectiveAllyType && cardMatches (CardWithTrait Ship) a)
            ships
        )
        $ push
        $ EliminatePlayer pid
    penalize target = case target of
      HeroTarget _ -> characterController target >>= traverse_ (`raiseThreat` 1)
      AllyTarget _ -> characterController target >>= traverse_ (`raiseThreat` 1)
      _ -> pure ()

-- The discarded non-locations remain in the discard pile; only the locations
-- found are added to staging (Standard Mode setup card).
startingLocations :: Int -> [EncounterCard] -> ([EncounterCard], [EncounterCard], [EncounterCard])
startingLocations n cards | n <= 0 = ([], [], cards)
startingLocations _ [] = ([], [], [])
startingLocations n (card : rest)
  | cdCardType (toCardDef card) == LocationType =
      let (found, discarded, remaining) = startingLocations (n - 1) rest
       in (card : found, discarded, remaining)
  | otherwise =
      let (found, discarded, remaining) = startingLocations n rest
       in (found, card : discarded, remaining)
