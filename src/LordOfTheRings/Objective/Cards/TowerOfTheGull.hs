module LordOfTheRings.Objective.Cards.TowerOfTheGull (towerOfTheGull) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype TowerOfTheGull = TowerOfTheGull ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

towerOfTheGull :: ObjectiveCard TowerOfTheGull
towerOfTheGull = objective TowerOfTheGull Cards.towerOfTheGull

prey :: EnemyMatcher
prey = shipEnemy <> not_ UniqueEnemy

-- | Guarded. If Tower of the Gull is free of encounters, add it to the staging area,
-- controlled by the players. Combat Action: If the players control Tower of the Gull,
-- exhaust it to deal 2 damage to a non-unique Ship enemy. Any player may trigger this
-- action.
instance HasAbilities TowerOfTheGull where
  getAbilities (TowerOfTheGull a) =
    [ anyPlayer
        $ restricted a 1 (maybe NoCriteria CardNotInPlay a.objectiveGuard <> DuringPhase CombatPhase <> exists prey)
        $ ActionAbility (exhaustCost a)
    ]

instance RunMessage TowerOfTheGull where
  runMessage msg o@(TowerOfTheGull attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      ships <- select prey
      chooseOrRunOneM pid do
        questionLabeled "towerOfTheGull.damage"
        causedBy attrs
        targets ships \e -> dealDamage attrs e 2
      pure o
    _ -> TowerOfTheGull <$> runMessage msg attrs
