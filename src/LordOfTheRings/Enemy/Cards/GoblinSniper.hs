module LordOfTheRings.Enemy.Cards.GoblinSniper (goblinSniper) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype GoblinSniper = GoblinSniper EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinSniper :: EnemyCard GoblinSniper
goblinSniper = enemy GoblinSniper Cards.goblinSniper

-- | During the encounter phase, players cannot optionally engage Goblin Sniper if there
-- are other enemies in the staging area.
instance HasModifiersFor GoblinSniper where
  getModifiersFor (GoblinSniper a) = modified a a [CannotBeOptionallyEngagedWhileOthersInStaging]

-- | Forced: If Goblin Sniper is in the staging area at the end of the combat phase, each
-- player deals 1 point of damage to 1 character he controls.
instance HasAbilities GoblinSniper where
  getAbilities (GoblinSniper a) =
    [restricted a 1 (EnemyExists $ EnemyWithId a.id <> EnemyInStagingArea) $ forced $ PhaseEndsWindow When CombatPhase]

instance RunMessage GoblinSniper where
  runMessage msg e@(GoblinSniper attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        characters <- charactersControlledBy p
        chooseOneM p do
          questionLabeled "goblinSniper.damage"
          causedBy attrs
          targets characters \c -> dealDamage attrs c 1
      pure e
    _ -> GoblinSniper <$> runMessage msg attrs
