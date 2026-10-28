module LordOfTheRings.Objective.Cards.AScrapOfHistory (aScrapOfHistory) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (claimClue)

newtype AScrapOfHistory = AScrapOfHistory ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aScrapOfHistory :: ObjectiveCard AScrapOfHistory
aScrapOfHistory = objective AScrapOfHistory Cards.aScrapOfHistory

-- | Action: Exhaust a hero to claim this objective, add it to the victory display, and
-- place 1 resource token on the current quest.
instance HasAbilities AScrapOfHistory where
  getAbilities (AScrapOfHistory a) =
    [ anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

instance RunMessage AScrapOfHistory where
  runMessage msg o@(AScrapOfHistory attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      claimClue attrs 1
      pure o
    _ -> AScrapOfHistory <$> runMessage msg attrs
