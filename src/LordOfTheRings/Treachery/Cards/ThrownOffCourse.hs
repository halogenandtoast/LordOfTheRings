module LordOfTheRings.Treachery.Cards.ThrownOffCourse (thrownOffCourse) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ThrownOffCourse = ThrownOffCourse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thrownOffCourse :: TreacheryCard ThrownOffCourse
thrownOffCourse = treachery ThrownOffCourse Cards.thrownOffCourse

-- | When Revealed: Either remove all progress from the current quest and raise each
-- player's threat by 4, or shift your heading off-course. The first player chooses.
-- Shadow: If this attack destroys a character, shift your heading off-course.
instance RunMessage ThrownOffCourse where
  runMessage msg t@(ThrownOffCourse attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "thrownOffCourse.choose"
        causedBy attrs
        labeled "thrownOffCourse.progress" do
          removeProgressFromQuest attrs maxBound
          eachPlayer \p -> encounterRaisesThreat p 4
        labeled "shiftHeadingOffCourse" $ shiftHeadingOffCourse attrs
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      shiftHeadingOffCourse attrs
      pure t
    _ -> ThrownOffCourse <$> runMessage msg attrs
