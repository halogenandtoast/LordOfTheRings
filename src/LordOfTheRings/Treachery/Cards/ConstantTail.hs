module LordOfTheRings.Treachery.Cards.ConstantTail (constantTail) where

import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (canRemoveQuestTimeCounter)
import LordOfTheRings.Trait (Trait (Spy))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ConstantTail = ConstantTail TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

constantTail :: TreacheryCard ConstantTail
constantTail = treachery ConstantTail Cards.constantTail

-- | When Revealed: Return all engaged enemies to the staging area. Then, the first player
-- either removes 1 time counter from the current quest or each Spy enemy in the staging
-- area gets +2 threat until the end of the phase.
instance RunMessage ConstantTail where
  runMessage msg t@(ConstantTail attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      returnEngagedEnemiesToStaging
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      lead <- getLead
      removable <- canRemoveQuestTimeCounter
      chooseOrRunOneM lead do
        questionLabeled "constantTail.choose"
        causedBy attrs
        when removable $ labeled "constantTail.removeTimeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
        labeled "constantTail.spies" do
          spies <- select (EnemyInStagingArea <> EnemyWithTrait Spy)
          for_ spies \e -> phaseModifier attrs e (ThreatModifier 2)
      pure t
    _ -> ConstantTail <$> runMessage msg attrs
