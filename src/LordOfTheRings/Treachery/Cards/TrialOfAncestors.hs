module LordOfTheRings.Treachery.Cards.TrialOfAncestors (trialOfAncestors) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (Meta (..), getMeta, sharingClanTrait)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TrialOfAncestors = TrialOfAncestors TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

trialOfAncestors :: TreacheryCard TrialOfAncestors
trialOfAncestors = treachery TrialOfAncestors Cards.trialOfAncestors

-- | Counts as a Condition attachment with the text: "Each non-unique character gets -1
-- willpower and cannot attack enemies that share a Trait with the current trial's key
-- objective."
instance HasModifiersFor TrialOfAncestors where
  getModifiersFor (TrialOfAncestors a) = case a.placement of
    AttachedTo _ -> do
      current <- getsGame ((.currentKey) . getMeta . toAttrs . (.scenario))
      let heldKeys = [k | k <- [Objectives.keyOfTheBoar, Objectives.keyOfTheWolf, Objectives.keyOfTheRaven], Just (toCardCode k) == current]
      allies <- select (NotAlly UniqueAlly)
      for_ allies \c -> modified a c $ WillpowerModifier (-1) : [CannotAttackMatching (sharingClanTrait heldKeys) | notNull heldKeys]
    _ -> pure ()

-- | When Revealed: Attach to the current quest.
instance RunMessage TrialOfAncestors where
  runMessage msg t@(TrialOfAncestors attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    _ -> TrialOfAncestors <$> runMessage msg attrs
