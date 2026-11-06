module LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost (theRuinsOfBelegost) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (CardId)
import LordOfTheRings.Modifier (ModifierType (AttackWasDealtBlankShadow))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (heroHasLoot)

newtype TheRuinsOfBelegost = TheRuinsOfBelegost ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRuinsOfBelegost :: Scenario
theRuinsOfBelegost = Scenario $ scenario TheRuinsOfBelegost "the-ruins-of-belegost" "The Ruins of Belegost"

-- Private metadata records blank shadows dealt for the next attack,
-- and whether one resolved in the current attack. The flag resets for every attack.
instance HasModifiersFor TheRuinsOfBelegost where
  getModifiersFor (TheRuinsOfBelegost a) = do
    let (_, active) = fromMaybe ([], False) (parseMaybe parseJSON a.scenarioMeta :: Maybe ([CardId], Bool))
    when active $ modified a GameTarget [AttackWasDealtBlankShadow]

instance RunMessage TheRuinsOfBelegost where
  runMessage msg (TheRuinsOfBelegost a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.TheRuinsOfBelegost]
      putQuestIntoPlay =<< genEncounterCard Quests.ruinsOfTheFirstAge
      later <- traverse genEncounterCard [Quests.ancientEvilsDisturbed, Quests.deeperIntoThePast, Quests.theServantsOfMorgoth, Quests.theRootsOfEredLuin, Quests.theMinesOfBelegost, Quests.theBeastOfBelegost]
      pure $ TheRuinsOfBelegost $ a & encounterDeckL .~ deck & questDeckL .~ later
    AttachShadowCard eid card -> do
      dragon <- getsGame (lookup eid . (.entities.enemies))
      let (pending, active) = fromMaybe ([], False) (parseMaybe parseJSON a.scenarioMeta :: Maybe ([CardId], Bool))
      pure $ TheRuinsOfBelegost $ if maybe False ((== toCardCode Enemies.naurlhug) . toCardCode) dragon
        then a & metaL .~ toJSON (pending <> [card.id | not (cdShadowEffect $ toCardDef card)], active)
        else a
    -- Caleb Grace, 5 Dec 2018, ALeP FAQ 3.273 #3: discarding a shadow
    -- before the attack prevents a "dealt a shadow with no effect" trigger.
    -- https://alongextendedparty.com/semi-official-errata-faq-1-10/
    EnemyAttack _ eid -> do
      dragon <- getsGame (lookup eid . (.entities.enemies))
      let (pending, _) = fromMaybe ([], False) (parseMaybe parseJSON a.scenarioMeta :: Maybe ([CardId], Bool))
      pure $ TheRuinsOfBelegost $ if maybe False ((== toCardCode Enemies.naurlhug) . toCardCode) dragon
        then a & metaL .~ toJSON (filter (\cid -> maybe False (\e -> any ((== cid) . (.id)) (toAttrs e).shadowCards) dragon) pending, False)
        else a
    RevealShadowCard eid cid -> do
      dragon <- getsGame (lookup eid . (.entities.enemies))
      let (pending, active) = fromMaybe ([], False) (parseMaybe parseJSON a.scenarioMeta :: Maybe ([CardId], Bool))
      pure $ TheRuinsOfBelegost $ if maybe False ((== toCardCode Enemies.naurlhug) . toCardCode) dragon
        then a & metaL .~ toJSON (filter (/= cid) pending, active || cid `elem` pending)
        else a
    DiscardShadowCards eid -> do
      dragon <- getsGame (lookup eid . (.entities.enemies))
      let (_, active) = fromMaybe ([], False) (parseMaybe parseJSON a.scenarioMeta :: Maybe ([CardId], Bool))
      pure $ TheRuinsOfBelegost $ if maybe False ((== toCardCode Enemies.naurlhug) . toCardCode) dragon
        then a & metaL .~ toJSON ([] :: [CardId], active)
        else a
    SetAsideEnemy eid -> do
      dragon <- getsGame (lookup eid . (.entities.enemies))
      pure $ TheRuinsOfBelegost $ if maybe False ((== toCardCode Enemies.naurlhug) . toCardCode) dragon
        then a & metaL .~ toJSON ([] :: [CardId], False)
        else a
    CompleteQuest qid -> do
      current <- getsGame (lookup qid . (.entities.quests))
      case current of
        Nothing -> TheRuinsOfBelegost <$> runMessage msg a
        Just q | toCardCode q `elem` map toCardCode [Quests.ruinsOfTheFirstAge, Quests.ancientEvilsDisturbed, Quests.deeperIntoThePast, Quests.theServantsOfMorgoth, Quests.theRootsOfEredLuin, Quests.theMinesOfBelegost] -> do
          let code = toCardCode q
              chooseBy loot yes no = do
                held <- heroHasLoot loot
                pure $ if held then yes else no
          next <- if code == toCardCode Quests.ruinsOfTheFirstAge then chooseBy Objectives.keysOfBelegost Quests.deeperIntoThePast Quests.ancientEvilsDisturbed
            else if code == toCardCode Quests.ancientEvilsDisturbed then chooseBy Objectives.bookOfAulesChildren Quests.theRootsOfEredLuin Quests.theServantsOfMorgoth
            else if code == toCardCode Quests.deeperIntoThePast then chooseBy Objectives.bookOfAulesChildren Quests.theMinesOfBelegost Quests.theRootsOfEredLuin
            else if code == toCardCode Quests.theServantsOfMorgoth then chooseBy Objectives.dwarvenTorch Quests.theBeastOfBelegost Quests.theRootsOfEredLuin
            else if code == toCardCode Quests.theRootsOfEredLuin then chooseBy Objectives.mapOfEredLuin Quests.theBeastOfBelegost Quests.theMinesOfBelegost
            else chooseBy Objectives.blueMountainGem Quests.theBeastOfBelegost Quests.theServantsOfMorgoth
          let deck = a.questDeck
              chosen = filter ((== toCardCode next) . toCardCode) deck
              remaining = filter ((/= toCardCode next) . toCardCode) deck
              reusable = [MkEncounterCard (toAttrs q).cardId (toCardCode q) | code `elem` map toCardCode [Quests.theServantsOfMorgoth, Quests.theRootsOfEredLuin, Quests.theMinesOfBelegost]]
          -- The scenario chooses the next physical stage before the normal
          -- CompleteQuest removes the old one and queues AdvanceQuest.
          pure $ TheRuinsOfBelegost $ a & questDeckL .~ (chosen <> remaining <> reusable)
        _ -> TheRuinsOfBelegost <$> runMessage msg a
    _ -> TheRuinsOfBelegost <$> runMessage msg a
