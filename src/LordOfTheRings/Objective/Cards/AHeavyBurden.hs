module LordOfTheRings.Objective.Cards.AHeavyBurden (aHeavyBurden) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (RingBearer))

newtype AHeavyBurden = AHeavyBurden ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Shelob's Lair burden set. Permanent. Setup: Attach to the Ring-bearer.
aHeavyBurden :: ObjectiveCard AHeavyBurden
aHeavyBurden = objective AHeavyBurden Cards.aHeavyBurden

-- | Forced: At the end of the planning phase, either raise each player's threat by 1, or
-- exhaust the Ring-bearer.
instance HasAbilities AHeavyBurden where
  getAbilities (AHeavyBurden a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | AttachedTo (HeroTarget _) <- [a.placement]]

-- | The attached hero's controller chooses; with no ready Ring-bearer, each player's threat
-- is raised.
instance RunMessage AHeavyBurden where
  runMessage msg o@(AHeavyBurden attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [attrs.placement]] \hid -> do
        you <- (.owner) <$> getHero hid
        ringBearers <- select (HeroWithTrait RingBearer <> ReadyHero)
        chooseOrRunOneM you do
          questionLabeled "aHeavyBurden.choose"
          causedBy attrs
          labeled "aHeavyBurden.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p 1
          targets ringBearers exhaust
      pure o
    _ -> AHeavyBurden <$> runMessage msg attrs
