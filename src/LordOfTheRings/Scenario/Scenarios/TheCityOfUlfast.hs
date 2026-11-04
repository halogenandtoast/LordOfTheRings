module LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast (theCityOfUlfast) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheCityOfUlfast = TheCityOfUlfast ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCityOfUlfast :: Scenario
theCityOfUlfast = Scenario $ scenario TheCityOfUlfast "the-city-of-ulfast" "The City of Ulfast"

{- | The City of Ulfast, Servants of Sauron and Under Guard make up the encounter deck (MEC79
rules). City Guard is set aside as the deck is gathered, for stage 1A's setup to add it to
the staging area.
-}
instance RunMessage TheCityOfUlfast where
  runMessage msg (TheCityOfUlfast attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheCityOfUlfast, EncounterSet.ServantsOfSauron, EncounterSet.UnderGuard]
      let (aside, encounterDeck) = partition (cardMatches $ cardIs Enemies.cityGuard) cards
      firstStage <- genEncounterCard Quests.searchingForUlchor
      laterStages <- traverse genEncounterCard [Quests.tryingToBlendIn, Quests.drawingAttention, Quests.ulchorsTrap]
      putQuestIntoPlay firstStage
      pure
        $ TheCityOfUlfast
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> TheCityOfUlfast <$> runMessage msg attrs
