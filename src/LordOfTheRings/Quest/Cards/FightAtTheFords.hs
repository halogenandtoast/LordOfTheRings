module LordOfTheRings.Quest.Cards.FightAtTheFords (fightAtTheFords) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers

newtype FightAtTheFords = FightAtTheFords QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fightAtTheFords :: QuestCard FightAtTheFords
fightAtTheFords = quest FightAtTheFords Cards.fightAtTheFords

-- | Time 5. Forced: After the last time counter is removed from this stage, discard Gríma
-- from play.
instance HasAbilities FightAtTheFords where
  getAbilities (FightAtTheFords a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a | a.flipped]

-- | Setup: Add The Islet to the staging area and attach Gríma to that location. Each
-- player searches the encounter deck for 1 different Dunland enemy and adds it to the
-- staging area. Shuffle the encounter deck.
-- The players cannot advance unless the first player controls Gríma: checked again as
-- Gríma comes under control and as the first player changes.
instance RunMessage FightAtTheFords where
  runMessage msg q@(FightAtTheFords attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copy def = take 1 $ filter ((== toCardCode def) . toCardCode) deck
      for_ (copy Locations.theIslet) \islet -> do
        removeFromEncounterDeck islet
        addToStaging islet
      -- Gríma is attached once The Islet is in play
      doStep 1 msg
      searchForDifferentDunlandEnemies attrs
      pure $ FightAtTheFords $ attrs {questFlipped = True}
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      islet <- select (LocationWithCardCode $ toCardCode Locations.theIslet)
      for_ (take 1 [c | c <- deck, toCardCode c == toCardCode Objectives.grimaTheFordsOfIsen]) \grima ->
        for_ (take 1 islet) \lid -> do
          removeFromEncounterDeck grima
          putObjectiveIntoPlayAttached grima (LocationTarget lid)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      traverse_ discard =<< getGrima
      pure q
    PutObjectiveAllyIntoPlay {} -> recheck
    TakeControl {} -> recheck
    SetFirstPlayer {} -> recheck
    _ -> FightAtTheFords <$> runConditionalStage firstPlayerControlsGrima msg attrs
   where
    recheck = do
      checkStage attrs.id
      FightAtTheFords <$> runConditionalStage firstPlayerControlsGrima msg attrs
