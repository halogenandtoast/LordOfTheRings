module LordOfTheRings.Enemy.Cards.OrcHunter (orcHunter) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcHunter = OrcHunter EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHunter :: EnemyCard OrcHunter
orcHunter = enemy OrcHunter Cards.orcHunter

-- | When Revealed: Either reveal an additional encounter card, or remove 1 time counter from
-- the current quest.
-- Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage OrcHunter where
  runMessage msg e@(OrcHunter attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      timed <- canRemoveTimeCounterFromCurrentQuest
      chooseOrRunOneM lead do
        questionLabeled "orcHunter.choose"
        causedBy attrs
        labeled "orcHunter.reveal" revealEncounterCard
        when timed $ labeled "removeTimeCounterFromQuest" $ removeTimeCountersFromCurrentQuest attrs 1
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> OrcHunter <$> runMessage msg attrs
