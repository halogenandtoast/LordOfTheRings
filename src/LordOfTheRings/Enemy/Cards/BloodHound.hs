module LordOfTheRings.Enemy.Cards.BloodHound (bloodHound) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BloodHound = BloodHound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bloodHound :: EnemyCard BloodHound
bloodHound = enemy BloodHound Cards.bloodHound

-- | Forced: When Blood Hound engages you, raise your threat by 5.
instance HasAbilities BloodHound where
  getAbilities (BloodHound a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

-- | Shadow: Either raise your threat by 10, or attacking enemy makes an additional attack
-- against you after this one.
instance RunMessage BloodHound where
  runMessage msg e@(BloodHound attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> encounterRaisesThreat pid 5
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "bloodHound.choose"
        causedBy attrs
        labeled "bloodHound.raiseThreat" $ encounterRaisesThreat pid 10
        labeled "bloodHound.attack" $ additionalAttack pid eid
      pure e
    _ -> BloodHound <$> runMessage msg attrs
