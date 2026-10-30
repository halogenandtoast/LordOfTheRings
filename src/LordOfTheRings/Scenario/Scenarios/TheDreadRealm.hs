module LordOfTheRings.Scenario.Scenarios.TheDreadRealm (theDreadRealm) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheDreadRealm = TheDreadRealm ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDreadRealm :: Scenario
theDreadRealm = Scenario $ scenario TheDreadRealm "the-dread-realm" "The Dread Realm"

{- | The encounter deck is built from The Dread Realm, Cursed Dead and Dark Sorcery.
Setup (The Catacombs of Carn Dûm): Set Daechanar and Altar of Midwinter aside, out of
play; the stage itself reveals and reanimates.
-}
instance RunMessage TheDreadRealm where
  runMessage msg (TheDreadRealm attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- concatMapM (gatherEncounterSet attrs) [EncounterSet.TheDreadRealm, EncounterSet.CursedDead, EncounterSet.DarkSorcery]
      let (aside, encounterDeck) = partition ((`elem` map toCardCode [Enemies.daechanar, Locations.altarOfMidwinter]) . toCardCode) cards
      firstStage <- genEncounterCard Quests.theCatacombsOfCarnDum
      laterStages <- traverse genEncounterCard [Quests.angmarAwakened, Quests.daechanarsFall]
      putQuestIntoPlay firstStage
      pure $ TheDreadRealm $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages & setAsideL .~ aside
    _ -> TheDreadRealm <$> runMessage msg attrs
