module LordOfTheRings.Scenario.Scenarios.TheMorgulVale (theMorgulVale) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (getPlayer, getRemainingPlayers)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheMorgulVale = TheMorgulVale ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMorgulVale :: Scenario
theMorgulVale = Scenario $ scenario TheMorgulVale "the-morgul-vale" "The Morgul Vale"

instance RunMessage TheMorgulVale where
  runMessage msg (TheMorgulVale attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheMorgulVale, EncounterSet.CreaturesOfTheForest, EncounterSet.MordorElite]
      firstStage <- genEncounterCard Quests.theRearguardTheMorgulVale
      laterStages <- traverse genEncounterCard [Quests.theBetrayal, Quests.theMorgulRider]
      putQuestIntoPlay firstStage
      -- To the Tower: "Remove Faramir from the game." Each copy leaves its deck before the
      -- starting hands are drawn.
      -- ponytail: a Faramir hero chosen to play stays in play; only the decks are searched.
      players <- getRemainingPlayers
      for_ players \pid -> do
        deck <- (.deck) <$> getPlayer pid
        for_ [c | c <- deck, isFaramir (cdName $ toCardDef c)] \c -> removeFromDeck pid c.id
      pure $ TheMorgulVale $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheMorgulVale <$> runMessage msg attrs
   where
    isFaramir (Name title _) = title == "Faramir"
