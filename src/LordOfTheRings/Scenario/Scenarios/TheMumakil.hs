module LordOfTheRings.Scenario.Scenarios.TheMumakil (theMumakil) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny, CardWithTrait), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (CaptureTrait))
import LordOfTheRings.Window (Window (..), WindowType (Explored))

newtype TheMumakil = TheMumakil ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMumakil :: Scenario
theMumakil = Scenario $ scenario TheMumakil "the-mumakil" "The Mûmakil"

{- | The encounter deck is built from The Mûmakil and Jungle Forest (rule sheet). Welcome
to the Jungle's "Set each copy of Wild Mûmak and each Capture objective aside, out of play"
is done as the deck is gathered, as The Drowned Ruins sets Shrine to Morgoth aside.
Its meta is whether a location was explored this round, for Territorial Ape's shadow.
-}
instance RunMessage TheMumakil where
  runMessage msg (TheMumakil attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheMumakil, EncounterSet.JungleForest]
      let (aside, encounterDeck) = partition (cardMatches $ CardMatchAny [cardIs Enemies.wildMumak, CardWithTrait CaptureTrait]) cards
      firstStage <- genEncounterCard Quests.welcomeToTheJungle
      laterStages <- traverse genEncounterCard [Quests.captureTheOliphaunts]
      putQuestIntoPlay firstStage
      pure
        $ TheMumakil
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    CheckWindows ws | notNull [() | Window _ (Explored _) <- ws] -> TheMumakil . (metaL .~ toJSON True) <$> runMessage msg attrs
    EndRound -> TheMumakil . (metaL .~ toJSON False) <$> runMessage msg attrs
    _ -> TheMumakil <$> runMessage msg attrs
