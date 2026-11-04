module LordOfTheRings.Quest.Cards.TheCrossroadsFort (theCrossroadsFort) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (haradStage, revealRandomHaradObjective)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype TheCrossroadsFort = TheCrossroadsFort QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrossroadsFort :: QuestCard TheCrossroadsFort
theCrossroadsFort = quest TheCrossroadsFort Cards.theCrossroadsFort

-- | If a Harad objective is not attached, the first player claims it and attaches it to a
-- hero he controls. This stage cannot be defeated unless each Harad objective in play is
-- attached to a hero.
instance HasModifiersFor TheCrossroadsFort where
  getModifiersFor (TheCrossroadsFort a) = when a.flipped $ haradStage a

{- | Setup: Set each Harad objective aside, out of play. Add The Black Serpent to the
staging area. Shuffle the encounter deck. The first player randomly selects 1 set aside
Harad objective, reveals it, and adds it to the staging area.
The scenario set the objectives and The Black Serpent aside as it gathered the deck.
-}
instance RunMessage TheCrossroadsFort where
  runMessage msg q@(TheCrossroadsFort attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ addToStaging $ filter (cardMatches $ cardIs Enemies.theBlackSerpent) aside
      shuffleEncounterDeck
      revealRandomHaradObjective
      TheCrossroadsFort <$> runMessage msg attrs
    _ -> TheCrossroadsFort <$> runMessage msg attrs
