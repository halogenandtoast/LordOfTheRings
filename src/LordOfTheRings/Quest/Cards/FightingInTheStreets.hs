module LordOfTheRings.Quest.Cards.FightingInTheStreets (fightingInTheStreets) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (getScroll, scrollOnHero)

newtype FightingInTheStreets = FightingInTheStreets QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fightingInTheStreets :: QuestCard FightingInTheStreets
fightingInTheStreets = quest FightingInTheStreets Cards.fightingInTheStreets

-- | Battle.
-- When Revealed: Attach Alcaron's Scroll to the highest engagement cost enemy in play, if
-- able. Otherwise, add Alcaron's Scroll to the staging area.
-- The players cannot advance to the next stage unless Alcaron's Scroll is attached to a
-- hero.
instance RunMessage FightingInTheStreets where
  runMessage msg q@(FightingInTheStreets attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      FightingInTheStreets <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      getScroll >>= traverse_ \scroll -> do
        enemies <- traverse (\e -> (e,) <$> getEngagementCost e) =<< select AnyEnemy
        let highest = foldr (max . snd) 0 enemies
        lead <- getLead
        if null enemies
          then placeObjective scroll StagingArea
          else chooseOrRunOneM lead do
            questionLabeled "fightingInTheStreets.attach"
            causedBy attrs
            targets [e | (e, cost) <- enemies, cost == highest] (attachObjective scroll)
      pure q
    _ -> FightingInTheStreets <$> runConditionalStage scrollOnHero msg attrs
