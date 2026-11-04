module LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor (theLongArmOfMordor) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (HeroMatcher (HeroInStagingArea), cardIs, cardMatches, getHero, select)
import LordOfTheRings.Modifier (ModifierType (Blank, ImmuneToPlayerCardEffects))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheLongArmOfMordor = TheLongArmOfMordor ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLongArmOfMordor :: Scenario
theLongArmOfMordor = Scenario $ scenario TheLongArmOfMordor "the-long-arm-of-mordor" "The Long Arm of Mordor"

-- | Stage 1A's lasting effect: "For the remainder of the game, heroes in the staging area
-- are in play but under no player's control, immune to player card effects, and their
-- text boxes are considered to be blank." Once a hero leaves the staging area it no longer
-- applies (MEC55 rules).
instance HasModifiersFor TheLongArmOfMordor where
  getModifiersFor (TheLongArmOfMordor a) = do
    heroes <- select HeroInStagingArea
    for_ heroes \h -> modified a h [Blank, ImmuneToPlayerCardEffects]

{- | The encounter deck is built from The Long Arm of Mordor, Mordor Orcs and Jungle
Forest. Uruk Chieftain and the four objective-heroes are set aside as it is gathered;
stage 1A's setup puts them to use.
-}
instance RunMessage TheLongArmOfMordor where
  runMessage msg (TheLongArmOfMordor attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheLongArmOfMordor, EncounterSet.MordorOrcs, EncounterSet.JungleForest]
      let asideCard c = cardMatches (cardIs Enemies.urukChieftain) c || cdCardType (toCardDef c) == ObjectiveHeroType
          (aside, encounterDeck) = partition asideCard cards
      firstStage <- genEncounterCard Quests.surpriseAttack
      laterStages <- traverse genEncounterCard [Quests.findTheOthers, Quests.freeTheCaptives, Quests.escapeIntoTheJungle]
      putQuestIntoPlay firstStage
      pure
        $ TheLongArmOfMordor
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    -- an eliminated player's heroes in the staging area leave the game with him
    EliminatePlayer pid -> do
      heroes <- filterM (fmap ((== pid) . (.owner)) . getHero) =<< select HeroInStagingArea
      for_ heroes discard
      TheLongArmOfMordor <$> runMessage msg attrs
    _ -> TheLongArmOfMordor <$> runMessage msg attrs
