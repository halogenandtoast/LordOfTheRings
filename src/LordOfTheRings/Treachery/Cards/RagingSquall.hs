module LordOfTheRings.Treachery.Cards.RagingSquall (ragingSquall) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RagingSquall = RagingSquall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ragingSquall :: TreacheryCard RagingSquall
ragingSquall = treachery RagingSquall Cards.ragingSquall

-- | Counts as a Condition attachment with the text: "Forced: After an ally is committed to
-- the quest, deal 1 damage to that ally if you are off-course."
instance HasAbilities RagingSquall where
  getAbilities (RagingSquall a) = [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow After | AttachedTo _ <- [a.placement]]

-- | Doomed 2.
-- When Revealed: Attach Raging Squall to the current quest.
instance RunMessage RagingSquall where
  runMessage msg t@(RagingSquall attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      whenM isOffCourse $ for_ [aid | Window _ (CommittedToQuest (AllyTarget aid)) <- ws] \aid -> dealDamage attrs (AllyTarget aid) 1
      pure t
    _ -> RagingSquall <$> runMessage msg attrs
