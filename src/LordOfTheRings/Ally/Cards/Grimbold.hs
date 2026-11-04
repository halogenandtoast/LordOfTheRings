module LordOfTheRings.Ally.Cards.Grimbold (grimbold) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (placementController)

newtype Grimbold = Grimbold AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grimbold :: AllyCard Grimbold
grimbold = ally Grimbold Cards.grimbold

engaged :: EnemyMatcher
engaged = EnemyEngagedWith AnyPlayer <> EnemyNotImmuneToPlayerCards

-- | Combat Action: Discard Grimbold to choose an enemy engaged with a player. That enemy
-- cannot attack that player this phase.
instance HasAbilities Grimbold where
  getAbilities (Grimbold a) =
    [ restricted a 1 (DuringPhase CombatPhase <> exists engaged) $ ActionAbility Free
    | Just _ <- [placementController a.placement]
    ]

instance RunMessage Grimbold where
  runMessage msg s@(Grimbold a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a
      enemies <- select engaged
      chooseOrRunOneM pid do
        questionLabeled "grimbold.chooseEnemy"
        causedBy a
        targets enemies (cannotAttackEngagedPlayer a)
      pure s
    _ -> Grimbold <$> runMessage msg a
