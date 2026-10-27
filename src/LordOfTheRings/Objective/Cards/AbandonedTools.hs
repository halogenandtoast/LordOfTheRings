module LordOfTheRings.Objective.Cards.AbandonedTools (abandonedTools) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype AbandonedTools = AbandonedTools ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

abandonedTools :: ObjectiveCard AbandonedTools
abandonedTools = objective AbandonedTools Cards.abandonedTools

-- | Action: Exhaust a hero to claim this objective if it has no encounters attached. Then,
-- attach Abandoned Tools to that hero. (If detached, return Abandoned Tools to the staging
-- area.) Its Refresh Action is Escape from Darkness's to give: see that stage.
instance HasAbilities AbandonedTools where
  getAbilities (AbandonedTools a) =
    [ anyPlayer
        $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> exists (HeroControlledBy ActivePlayer <> ReadyHero))
        $ ActionAbility Free
    | a.placement == StagingArea
    ]

instance RunMessage AbandonedTools where
  runMessage msg o@(AbandonedTools attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOneM pid do
        questionLabeled "abandonedTools.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          attachObjective attrs.id h
      pure o
    _ -> AbandonedTools <$> runMessage msg attrs
