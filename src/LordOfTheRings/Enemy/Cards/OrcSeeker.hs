module LordOfTheRings.Enemy.Cards.OrcSeeker (orcSeeker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (lowerEliminationLevelsBy)

newtype OrcSeeker = OrcSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcSeeker :: EnemyCard OrcSeeker
orcSeeker = enemy OrcSeeker Cards.orcSeeker

-- | When Revealed: Either reduce each player's threat elimination level by 2, or Orc Seeker
-- engages the first player and makes an immediate attack.
-- Shadow: Reduce each player's threat elimination level by 2 (5 instead if undefended).
instance RunMessage OrcSeeker where
  runMessage msg e@(OrcSeeker attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "orcSeeker.choose"
        causedBy attrs
        labeled "orcSeeker.lowerElimination" $ lowerEliminationLevelsBy 2
        labeled "orcSeeker.attack" do
          engageEnemy lead attrs.id
          immediateAttack lead attrs.id
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      lowerEliminationLevelsBy (ifUndefended defenders 5 2)
      pure e
    _ -> OrcSeeker <$> runMessage msg attrs
