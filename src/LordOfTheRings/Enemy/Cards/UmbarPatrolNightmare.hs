module LordOfTheRings.Enemy.Cards.UmbarPatrolNightmare (umbarPatrolNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Raider))

newtype UmbarPatrolNightmare = UmbarPatrolNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarPatrolNightmare :: EnemyCard UmbarPatrolNightmare
umbarPatrolNightmare = enemy UmbarPatrolNightmare Cards.umbarPatrolNightmare

-- | Forced: At the end of the encounter phase, if Umbar Patrol is in the staging area, add 2
-- resources to the Raider enemy with the most resources on it.
instance HasAbilities UmbarPatrolNightmare where
  getAbilities (UmbarPatrolNightmare a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase | a.placement == StagingArea]

-- | Shadow: Add 1 resource to each Raider enemy engaged with you.
instance RunMessage UmbarPatrolNightmare where
  runMessage msg e@(UmbarPatrolNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      raiders <- traverse getEnemy =<< select (EnemyWithTrait Raider)
      let most = foldr (max . (.resources)) 0 raiders
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "umbarPatrolNightmare.raider"
        causedBy attrs
        targets [r.id | r <- raiders, r.resources == most] \r -> placeResources attrs r 2
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      raiders <- select (EnemyWithTrait Raider <> EnemyEngagedWith (PlayerWithId pid))
      for_ raiders \r -> placeResources (CardIdSource cid) r 1
      pure e
    _ -> UmbarPatrolNightmare <$> runMessage msg attrs
