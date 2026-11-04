module LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow (theLandOfSorrow) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (Camp, Captive, Warden))

newtype TheLandOfSorrow = TheLandOfSorrow ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLandOfSorrow :: Scenario
theLandOfSorrow = Scenario $ scenario TheLandOfSorrow "the-land-of-sorrow" "The Land of Sorrow"

{- | The Land of Sorrow, Easterling Raiders and Rolling Plains make up the encounter deck
(MEC82 rules). The Camp locations, the Captive objective allies and the Warden enemies are
set aside for stage 1A to place.
-}
instance RunMessage TheLandOfSorrow where
  runMessage msg (TheLandOfSorrow attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheLandOfSorrow, EncounterSet.EasterlingRaiders, EncounterSet.RollingPlains]
      let aside c = any (`member` cdCardTraits (toCardDef c)) [Camp, Captive, Warden]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.slavesOfTheDarkLord
      secondStage <- genEncounterCard Quests.leadTheRevolt
      putQuestIntoPlay firstStage
      pure
        $ TheLandOfSorrow
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ [secondStage]
    _ -> TheLandOfSorrow <$> runMessage msg attrs
