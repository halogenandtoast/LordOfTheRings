module LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion (roamAcrossRhovanion) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EredMithrinCampaign (eredMithrinAbilities, eredMithrinModifiers, runEredMithrinCampaign)

newtype RoamAcrossRhovanion = RoamAcrossRhovanion ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

roamAcrossRhovanion :: Scenario
roamAcrossRhovanion = Scenario $ scenario RoamAcrossRhovanion "roam-across-rhovanion" "Roam Across Rhovanion"

-- | The Ered Mithrin campaign card's abilities and modifiers, in campaign mode.
instance HasAbilities RoamAcrossRhovanion where
  getAbilities (RoamAcrossRhovanion a) = eredMithrinAbilities a

instance HasModifiersFor RoamAcrossRhovanion where
  getModifiersFor (RoamAcrossRhovanion a) = eredMithrinModifiers a

{- | The encounter deck is built from Roam Across Rhovanion, Fell Beasts, Lost in Wilderland
and Hills of Wilderland (MEC67 rules). Its objectives and objective-allies are set aside as
it is gathered; stage 1A's setup gives Urdug to the first player.

The quest deck holds the three stages 2 and stage 3. Each stage 2 is drawn at random from
those left, a defeated one leaving the game, and stage 3 comes once none are left: the next
stage is drawn at random from the lowest number left.
-}
instance RunMessage RoamAcrossRhovanion where
  runMessage msg (RoamAcrossRhovanion attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.RoamAcrossRhovanion, EncounterSet.FellBeasts, EncounterSet.LostInWilderland, EncounterSet.HillsOfWilderland]
      let (aside, encounterDeck) = partition ((`elem` [ObjectiveType, ObjectiveAllyType]) . cdCardType . toCardDef) cards
      firstStage <- genEncounterCard Quests.theGoblinsTask
      laterStages <- traverse genEncounterCard [Quests.rescueTiny, Quests.retrieveUrdugsHorn, Quests.findDurinsKey, Quests.urdugsGambit]
      putQuestIntoPlay firstStage
      pure
        $ RoamAcrossRhovanion
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    AdvanceQuest | Just lowest <- minimumMay (map stageOf attrs.questDeck) -> do
      next <- uniform (filter ((== lowest) . stageOf) attrs.questDeck)
      advanceToQuest next
      pure $ RoamAcrossRhovanion $ attrs & questDeckL %~ filter (/= next)
    _ -> RoamAcrossRhovanion <$> runEredMithrinCampaign msg attrs
   where
    stageOf = fromMaybe 1 . cdStage . toCardDef
