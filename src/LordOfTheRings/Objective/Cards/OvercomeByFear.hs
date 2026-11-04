module LordOfTheRings.Objective.Cards.OvercomeByFear (overcomeByFear) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype OvercomeByFear = OvercomeByFear ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Setup: Attach to your threat dial. Limit 1 per threat dial. (Stage 1A, the scenario
-- runner for the campaign pool's copies, and the cards that attach a set aside copy do.)
overcomeByFear :: ObjectiveCard OvercomeByFear
overcomeByFear = objective OvercomeByFear Cards.overcomeByFear

dialOf :: ObjectiveAttrs -> Maybe PlayerId
dialOf a = case a.placement of
  AttachedTo (PlayerTarget pid) -> Just pid
  _ -> Nothing

-- | Your threat cannot be reduced.
instance HasModifiersFor OvercomeByFear where
  getModifiersFor (OvercomeByFear a) = for_ (dialOf a) \pid -> modified a (PlayerTarget pid) [CannotReduceThreat]

-- | Forced: At the end of the round, raise your threat by 1.
instance HasAbilities OvercomeByFear where
  getAbilities (OvercomeByFear a) = [mkAbility a 1 $ forced RoundEndsWindow | isJust (dialOf a)]

-- | Then, you may spend 1 Fellowship resource to detach Overcome by Fear and set it aside,
-- out of play.
instance RunMessage OvercomeByFear where
  runMessage msg o@(OvercomeByFear a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ (dialOf a) \pid -> do
        encounterRaisesThreat pid 1
        doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      for_ (dialOf a) \pid -> do
        fellowship <- getResourcesOfSphere pid Fellowship
        when (fellowship > 0) do
          chooseOneM pid do
            questionLabeled "overcomeByFear.detach"
            causedBy a
            labeled "overcomeByFear.spend" do
              spendResources pid Fellowship 1
              setAsideObjective a.id
            labeled "pass" $ pure ()
      pure o
    _ -> OvercomeByFear <$> runMessage msg a
