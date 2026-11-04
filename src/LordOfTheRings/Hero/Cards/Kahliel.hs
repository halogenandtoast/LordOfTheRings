module LordOfTheRings.Hero.Cards.Kahliel (kahliel) where

import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (eliminatedIfLeavesPlay)

newtype Kahliel = Kahliel HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Objective-hero.
kahliel :: HeroCard Kahliel
kahliel = hero Kahliel Cards.kahliel

-- | Response: At the beginning of the planning phase, add 1 resource to a hero's resource
-- pool.
instance HasAbilities Kahliel where
  getAbilities (Kahliel a) = [mkAbility a 1 $ response $ PhaseBeginsWindow After PlanningPhase]

-- | If Kahliel leaves play, you are eliminated from the game.
-- A hero in the staging area is under no one's control and immune to player card
-- effects: the resource goes to a hero a player controls.
instance RunMessage Kahliel where
  runMessage msg h@(Kahliel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy AnyPlayer)
      chooseOrRunOneM pid do
        questionLabeled "kahliel.resource"
        causedBy attrs
        targets heroes \hid -> placeResources attrs hid 1
      pure h
    _ -> do
      eliminatedIfLeavesPlay attrs msg
      Kahliel <$> runMessage msg attrs
