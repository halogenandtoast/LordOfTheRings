module LordOfTheRings.Quest.Cards.SearchingTheIsland (searchingTheIsland) where

import LordOfTheRings.Card (EncounterCard, genEncounterCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.IslandMap (locationAt)
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveId))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived.Helpers (getGateKey)

newtype SearchingTheIsland = SearchingTheIsland QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingTheIsland :: QuestCard SearchingTheIsland
searchingTheIsland = quest SearchingTheIsland Cards.searchingTheIsland

{- | Setup: Prepare the Island Map (see insert). Flip over the top-left and bottom-left
locations of the Island Map (without resolving their Forced effects, if any), and choose
one as the players' starting active location. The first player takes control of Captain
Sahír, and the last player takes control of Na'asiyah (objective-ally sides faceup). Add
Temple Guardian to the staging area and attach Gate Key to it, guarded. Shuffle the
encounter deck. The scenario laid out the Island Map as it gathered the encounter deck;
the corners' Forced effects answer only while active, so they do not answer here.

Exploration. For each progress that would be placed on Searching the Island, you may place
it on a location in the staging area, instead: the first player places it one at a time,
or lets the rest go on the quest.
-}
instance RunMessage SearchingTheIsland where
  runMessage msg q@(SearchingTheIsland attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      corners <- map (.id) . catMaybes <$> traverse locationAt [Pos 0 0, Pos 0 2]
      traverse_ flipOver corners
      lead <- getLead
      chooseOneM lead do
        questionLabeled "searchingTheIsland.start"
        causedBy attrs
        targets corners makeActive
      traverse_ (addToStaging <=< genEncounterCard) [Objectives.captainSahirObjective, Objectives.naasiyahObjective]
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (filter (isOneOf [Enemies.templeGuardian, Objectives.gateKey]) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      push $ DoStep 1 msg
      shuffleEncounterDeck
      SearchingTheIsland <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      guardian <- selectOne (EnemyWithCardCode $ toCardCode Enemies.templeGuardian)
      key <- getGateKey
      for_ ((,) <$> key <*> guardian) \(k, g) -> attachObjective (objectiveId k) g
      pure q
    PlaceTokens source (isTarget attrs -> True) Progress n | attrs.flipped, n > 0 -> do
      open <- filterM hasRoom =<< select LocationInStagingArea
      if null open
        then SearchingTheIsland <$> runMessage msg attrs
        else do
          lead <- getLead
          chooseOneM lead do
            questionLabeledWith "searchingTheIsland.progress" [intVar "count" n]
            causedBy attrs
            targets open \l -> do
              placeProgress attrs l 1
              when (n > 1) $ placeProgress source attrs (n - 1)
            labeled "searchingTheIsland.onQuest" $ push (DoStep 1 msg)
          pure q
    DoStep 1 inner@(PlaceTokens _ (isTarget attrs -> True) Progress _) -> SearchingTheIsland <$> runMessage inner attrs
    _ -> SearchingTheIsland <$> runMessage msg attrs
   where
    isOneOf defs (c :: EncounterCard) = toCardCode c `elem` map toCardCode defs
    hasRoom lid = do
      l <- getLocation lid
      questPoints <- getLocationQuestPoints lid
      pure $ questPoints == 0 || l.progress < questPoints
