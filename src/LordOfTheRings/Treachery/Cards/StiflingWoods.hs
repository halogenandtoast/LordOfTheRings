module LordOfTheRings.Treachery.Cards.StiflingWoods (stiflingWoods) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StiflingWoods = StiflingWoods TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stiflingWoods :: TreacheryCard StiflingWoods
stiflingWoods = treachery StiflingWoods Cards.stiflingWoods

-- | When Revealed: Either remove 6 progress from the current quest, or remove 1 time
-- counter from the current quest. The first player chooses.
-- Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, remove all
-- progress from the current quest.
instance RunMessage StiflingWoods where
  runMessage msg t@(StiflingWoods attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "stiflingWoods.choose"
        causedBy attrs
        labeled "stiflingWoods.progress" $ removeProgressFromQuest attrs 6
        labeled "stiflingWoods.timeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      removeProgressFromQuest attrs maxBound
      pure t
    _ -> StiflingWoods <$> runMessage msg attrs
