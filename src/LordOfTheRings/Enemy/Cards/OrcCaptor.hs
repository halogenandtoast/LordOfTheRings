module LordOfTheRings.Enemy.Cards.OrcCaptor (orcCaptor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)

newtype OrcCaptor = OrcCaptor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcCaptor :: EnemyCard OrcCaptor
orcCaptor = enemy OrcCaptor Cards.orcCaptor

-- | Forced: At the end of the round, if Orc Captor is in the staging area, place 1 progress
-- token on To the Tower.
instance HasAbilities OrcCaptor where
  getAbilities (OrcCaptor a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

-- | Shadow: Attacking enemy gets +3 defense until the end of the combat phase.
instance RunMessage OrcCaptor where
  runMessage msg e@(OrcCaptor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeOnTower attrs 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 3)
      pure e
    _ -> OrcCaptor <$> runMessage msg attrs
