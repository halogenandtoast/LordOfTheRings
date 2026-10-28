module LordOfTheRings.Objective.Cards.APrisoner (aPrisoner) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (claimClue)

newtype APrisoner = APrisoner ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aPrisoner :: ObjectiveCard APrisoner
aPrisoner = objective APrisoner Cards.aPrisoner

-- | Action: Exhaust a hero to claim this objective, add it to the victory display, and
-- place 2 resource tokens on the current quest.
instance HasAbilities APrisoner where
  getAbilities (APrisoner a) =
    [ anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

instance RunMessage APrisoner where
  runMessage msg o@(APrisoner attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      claimClue attrs 2
      pure o
    _ -> APrisoner <$> runMessage msg attrs
