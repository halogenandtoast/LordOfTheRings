module LordOfTheRings.Quest.Cards.IntoTheWoods (intoTheWoods) where

import LordOfTheRings.Card (genEncounterCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (mugashOnHero, shuffleMugashIntoEncounterDeck)

newtype IntoTheWoods = IntoTheWoods QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheWoods :: QuestCard IntoTheWoods
intoTheWoods = quest IntoTheWoods Cards.intoTheWoods

-- | Forced: After the last time counter is removed from this stage, shuffle Mugash into the
-- encounter deck and advance to stage 3.
instance HasAbilities IntoTheWoods where
  getAbilities (IntoTheWoods a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Setup: Add Edge of Fangorn to the staging area and attach Mugash to that location.
-- Shuffle the encounter deck. Reveal X additional encounter cards where X equals the number
-- of players in the game minus 1.
-- The players cannot advance to stage 2 unless Mugash is attached to a hero.
instance RunMessage IntoTheWoods where
  runMessage msg q@(IntoTheWoods attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      edge <- headMay <$> copies Locations.edgeOfFangorn
      for_ edge \card -> do
        removeFromEncounterDeck card
        addToStaging card
      doStep 1 msg
      shuffleEncounterDeck
      players <- length <$> getRemainingPlayers
      revealEncounterCards (players - 1)
      turnOver attrs
      pure q
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      edge <- selectOne (LocationWithCardCode $ toCardCode Locations.edgeOfFangorn)
      mugash <- headMay <$> copies Objectives.mugashObjective
      for_ ((,) <$> edge <*> mugash) \(lid, card) -> do
        removeFromEncounterDeck card
        putObjectiveIntoPlayAttached card lid
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleMugashIntoEncounterDeck
      stage3 <- genEncounterCard Cards.theAngryForest
      resetToStage stage3 []
      pure q
    _ -> IntoTheWoods <$> runConditionalStage mugashOnHero msg attrs
   where
    copies def = getsGame (filter ((== toCardCode def) . toCardCode) . (.encounterDeck) . toAttrs . (.scenario))
