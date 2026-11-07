module LordOfTheRings.Scenario.Scenarios.FogOnTheBarrowDowns (fogOnTheBarrowDowns) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getGame, getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Game.Base (allPlayers, gameSeparation, Separation (separationFirstPlayer), seatsFrom)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Id (QuestId, PlayerId)
import LordOfTheRings.Matcher (getRemainingPlayers, select, LocationMatcher (LocationInStagingArea, LocationWithCardCode))
import LordOfTheRings.Modifier (ModifierType (Blank, CannotBeDefeated, PreserveStageWhenEmpty))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FogOnTheBarrowDowns.Helpers (homeQuest)

newtype FogOnTheBarrowDowns = FogOnTheBarrowDowns ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fogOnTheBarrowDowns :: Scenario
fogOnTheBarrowDowns = Scenario $ scenario FogOnTheBarrowDowns "fog-on-the-barrow-downs" "Fog on the Barrow-downs"

-- A return/elimination is one effect. Keep the home stage pending until all its
-- consequences resolve, including the Great Barrow leaving, before checking victory.
pending :: ScenarioAttrs -> Maybe (QuestId, Int)
pending a = join $ parseMaybe parseJSON a.scenarioMeta

beginChange :: QuestId -> ScenarioAttrs -> ScenarioAttrs
beginChange qid a = a & metaL .~ toJSON (Just (qid, maybe 1 ((+ 1) . snd) $ pending a))

endChange :: ScenarioAttrs -> ScenarioAttrs
endChange a = a & metaL .~ toJSON (pending a >>= \(qid, n) -> if n > 1 then Just (qid, n - 1) else Nothing)

instance HasModifiersFor FogOnTheBarrowDowns where
  getModifiersFor (FogOnTheBarrowDowns a) = do
    home <- homeQuest
    current <- getsGame (any ((== toCardCode Quests.lostInTheFog) . toCardCode) . (.entities.quests))
    -- The rules insert's instruction to retain stage 3B applies even if its text is blank.
    when (isJust home && current) $ modified a GameTarget [PreserveStageWhenEmpty]
    for_ (pending a) $ \(qid, _) -> modified a (QuestTarget qid) [CannotBeDefeated]

remainingAnywhere :: ReverseQueue m => m [PlayerId]
remainingAnywhere = do
  g <- getGame
  let token = maybe g.firstPlayer separationFirstPlayer (gameSeparation g)
      living = [p.id | p <- toList (allPlayers g), not p.eliminated]
  pure $ filter (`elem` living) (seatsFrom token g.playerOrder)

retireBarrow :: ReverseQueue m => ScenarioAttrs -> m ()
retireBarrow a = do
  players <- getRemainingPlayers
  elsewhere <- remainingAnywhere
  for_ (headMay $ players <> elsewhere) $ \pid -> do
    barrows <- select $ LocationInStagingArea <> LocationWithCardCode (toCardCode Locations.greatBarrow)
    chooseOrRunOneM pid do
      questionLabeled "fog.removeGreatBarrow"
      causedBy a
      -- Caleb Grace, 24 March 2015, ALeP FAQ 3.181: one Great Barrow from
      -- play goes to victory whenever a stage 4B leaves, including elimination.
      -- https://alongextendedparty.com/semi-official-errata-faq-1-10/
      targets barrows addToVictoryDisplay

instance RunMessage FogOnTheBarrowDowns where
  runMessage msg s@(FogOnTheBarrowDowns a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.FogOnTheBarrowDowns]
      putQuestIntoPlay =<< genEncounterCard Quests.theHouseOfTomBombadil
      later <- traverse genEncounterCard [Quests.acrossTheDowns, Quests.lostInTheFog]
      barrows <- replicateM 4 $ genEncounterCard Quests.trappedInsideABarrow
      pure $ FogOnTheBarrowDowns $ a & encounterDeckL .~ deck & questDeckL .~ later & setAsideL .~ barrows
    JoinQuestArea _ qid -> do
      current <- getsGame (find ((== toCardCode Quests.trappedInsideABarrow) . toCardCode) . toList . (.entities.quests))
      case current of
        Nothing -> pure s
        Just stage -> do
          setAside $ MkEncounterCard (toAttrs stage).cardId (toCardCode stage)
          doStep 1 msg
          pure $ FogOnTheBarrowDowns $ beginChange qid a
    DoStep 1 (JoinQuestArea _ _) -> do
      retireBarrow a
      doStep 2 msg
      pure s
    DoStep 2 (DoStep 1 (JoinQuestArea _ _)) -> pure $ FogOnTheBarrowDowns $ endChange a
    EliminatePlayer pid -> do
      home <- homeQuest
      g <- getGame
      let wasPlaying = maybe False (not . (.eliminated)) (lookup pid $ allPlayers g)
          trapped = find ((== toCardCode Quests.trappedInsideABarrow) . toCardCode) (toList g.entities.quests)
      case home of
        Just stage | wasPlaying -> do
          for_ trapped $ \q -> setAside $ MkEncounterCard (toAttrs q).cardId (toCardCode q)
          focusQuestArea stage.id
          push $ DoStep (if isJust trapped then 3 else 4) msg
          pure $ FogOnTheBarrowDowns $ beginChange stage.id a
        _ -> pure s
    DoStep step inner@(EliminatePlayer _) | step `elem` [3, 4] -> do
      home <- homeQuest
      for_ home $ \stage -> do
        mods <- getModifiers (QuestTarget stage.id)
        when (stage.flipped && Blank `notElem` mods) $ eachPlayer $ \pid -> encounterRaisesThreat pid 10
      when (step == 3) $ retireBarrow a
      push $ DoStep 5 inner
      pure s
    DoStep 5 (EliminatePlayer _) -> do
      atHome <- getRemainingPlayers
      others <- remainingAnywhere
      traverse_ focusAreaOf $ take 1 (atHome <> others)
      pure $ FogOnTheBarrowDowns $ endChange a
    _ -> FogOnTheBarrowDowns <$> runMessage msg a
