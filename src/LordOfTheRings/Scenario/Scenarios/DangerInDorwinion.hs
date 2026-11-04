module LordOfTheRings.Scenario.Scenarios.DangerInDorwinion (dangerInDorwinion) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DangerInDorwinion.Helpers (isPlotOrCultist)

newtype DangerInDorwinion = DangerInDorwinion ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dangerInDorwinion :: Scenario
dangerInDorwinion = Scenario $ scenario DangerInDorwinion "danger-in-dorwinion" "Danger in Dorwinion"

{- | The encounter deck is built from Danger in Dorwinion, Servants of Sauron, City of Rhûn
and Under Guard (MEC77 rules). Stage 1A sets each Plot objective and each Cultist enemy
aside, out of play: they are set aside as the deck is gathered. The players win as the
Cultist Leader of the Cult adds is defeated.
-}
instance RunMessage DangerInDorwinion where
  runMessage msg (DangerInDorwinion attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.DangerInDorwinion, EncounterSet.ServantsOfSauron, EncounterSet.CityOfRhun, EncounterSet.UnderGuard]
      let (aside, encounterDeck) = partition isPlotOrCultist cards
      firstStage <- genEncounterCard Quests.aCityInTurmoil
      laterStages <- traverse genEncounterCard [Quests.theShadowOfMordor, Quests.leaderOfTheCult]
      putQuestIntoPlay firstStage
      pure
        $ DangerInDorwinion
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    _ -> DangerInDorwinion <$> runMessage msg attrs
