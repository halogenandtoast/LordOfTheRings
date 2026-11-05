module LordOfTheRings.Quest.Cards.AmbushTheMenOfHarad (ambushTheMenOfHarad) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType), cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (revealFromUnderTheCrossRoads, theCrossRoads)

newtype AmbushTheMenOfHarad = AmbushTheMenOfHarad QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambushTheMenOfHarad :: QuestCard AmbushTheMenOfHarad
ambushTheMenOfHarad = quest AmbushTheMenOfHarad Cards.ambushTheMenOfHarad

-- | The Men of Harad encounter set, set aside by the scenario.
menOfHarad :: CardMatcher
menOfHarad = CardMatchAny $ map cardIs [Enemies.oliphaunt, Enemies.haradrimSoldier, Enemies.haradrimArcher]

-- | Forced: At the beginning of the quest phase, reveal the top facedown card from under
-- The Cross-roads and add it to the staging area.
instance HasAbilities AmbushTheMenOfHarad where
  getAbilities (AmbushTheMenOfHarad a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

{- | Setup: Add The Black Gate and The Cross-roads to the staging area. Shuffle the Men of
Harad encounter set and place it facedown under The Cross-roads. Each player, except the
first player, searches the encounter deck for a different non-unique location and adds it
to the staging area. Shuffle the encounter deck. One player after another, each searching
the deck as the one before left it.

The players cannot defeat this stage while The Cross-roads is in play. When The Cross-roads
is explored, the players win the game.
-}
instance RunMessage AmbushTheMenOfHarad where
  runMessage msg q@(AmbushTheMenOfHarad attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Objectives.theBlackGate)
      addSetAsideToStaging (cardIs Locations.theCrossRoadsTheLandOfShadow)
      doStep 1 msg
      AmbushTheMenOfHarad <$> runMessage msg attrs
    DoStep 1 inner@(PerformSetup (isTarget attrs -> True)) -> do
      crossRoads <- select theCrossRoads
      men <- getSetAside menOfHarad
      nightmare <- inNightmareMode
      warriors <- if nightmare then getSetAside (cardIs Enemies.southronWarrior) else pure []
      players <- length <$> getRemainingPlayers
      harad <- shuffleM (men <> take players warriors)
      for_ crossRoads \l -> placeUnderneath l (map EncounterCard harad)
      doStep 2 inner
      pure q
    DoStep n inner@(PerformSetup (isTarget attrs -> True)) -> do
      players <- drop 1 <$> getRemainingPlayers
      case drop (n - 2) players of
        pid : _ -> do
          deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
          staged <- traverse getLocation =<< select LocationInStagingArea
          let taken = map (cdName . toCardDef) staged
              locations =
                [ c
                | c <- deck
                , cardMatches (CardWithType LocationType <> NotCard UniqueCard) c
                , cdName (toCardDef c) `notElem` taken
                ]
          chooseOrRunOneM pid do
            questionLabeled "ambushTheMenOfHarad.search"
            showing (map EncounterCard locations)
            for_ locations \c -> targeting (CardIdTarget c.id) do
              removeFromEncounterDeck c
              addToStaging c
          doStep (n + 1) inner
        [] -> shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealFromUnderTheCrossRoads Nothing
      pure q
    CheckWindows ws | attrs.flipped, any (cardMatches $ cardIs Locations.theCrossRoadsTheLandOfShadow) [c | Window After (ExploredCard c) <- ws] -> do
      winGame
      AmbushTheMenOfHarad <$> runMessage msg attrs
    _ -> AmbushTheMenOfHarad <$> runConditionalStage (selectNone theCrossRoads) msg attrs
