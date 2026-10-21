module LordOfTheRings.Objective.Cards.Athelas (athelas) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Athelas = Athelas ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

athelas :: ObjectiveCard Athelas
athelas = objective Athelas Cards.athelas

-- | Action: Exhaust a hero to claim this objective if it has no encounters attached. Then,
-- attach Athelas to that hero.
instance HasAbilities Athelas where
  getAbilities (Athelas a) =
    [ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> exists (HeroControlledBy FirstPlayer <> ReadyHero))
        $ ActionAbility Free
    | a.placement == StagingArea
    ]

instance RunMessage Athelas where
  runMessage msg o@(Athelas attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOneM pid do
        questionLabeled "athelas.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          attachObjective attrs.id h
      pure o
    _ -> Athelas <$> runMessage msg attrs
