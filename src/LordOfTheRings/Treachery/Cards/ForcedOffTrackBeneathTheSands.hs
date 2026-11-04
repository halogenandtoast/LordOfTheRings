module LordOfTheRings.Treachery.Cards.ForcedOffTrackBeneathTheSands (forcedOffTrackBeneathTheSands) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (flipSearchTo, getSearchObjective, isOnTrack)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ForcedOffTrackBeneathTheSands = ForcedOffTrackBeneathTheSands TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forcedOffTrackBeneathTheSands :: TreacheryCard ForcedOffTrackBeneathTheSands
forcedOffTrackBeneathTheSands = treachery ForcedOffTrackBeneathTheSands Cards.forcedOffTrackBeneathTheSands

{- | When Revealed: Either flip the Search objective to Off Track, or deal 1 damage to each
questing character. The first player chooses; the flip is offered only if it would happen,
On Track faceup and free to flip (FAQ 1.44: one option must be performed in full, if able).
Shadow: If On Track is in play, attacking enemy makes an additional attack against you
after this one.
-}
instance RunMessage ForcedOffTrackBeneathTheSands where
  runMessage msg t@(ForcedOffTrackBeneathTheSands attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      on <- isOnTrack
      search <- getSearchObjective
      barred <- maybe (pure True) (fmap (elem CannotBeFlipped) . getModifiers) search
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "forcedOffTrack.choose"
        causedBy attrs
        when (on && not barred) $ labeled "forcedOffTrack.flip" $ flipSearchTo Objectives.offTrack
        labeled "forcedOffTrack.damage" do
          questing <- selectCharacters CommittedHero CommittedAlly
          for_ questing \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM isOnTrack $ additionalAttack pid eid
      pure t
    _ -> ForcedOffTrackBeneathTheSands <$> runMessage msg attrs
