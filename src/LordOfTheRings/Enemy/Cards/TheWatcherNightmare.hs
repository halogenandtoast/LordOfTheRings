module LordOfTheRings.Enemy.Cards.TheWatcherNightmare (theWatcherNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype TheWatcherNightmare = TheWatcherNightmare EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWatcherNightmare :: EnemyCard TheWatcherNightmare
theWatcherNightmare = enemy TheWatcherNightmare Cards.theWatcherNightmare

-- | While The Watcher is in the staging area, it cannot be damaged by player card effects.
-- ponytail: immune to player card effects there, not only to their damage
instance HasModifiersFor TheWatcherNightmare where
  getModifiersFor (TheWatcherNightmare a) = when (a.placement == StagingArea) $ modifySelf a [ImmuneToPlayerCardEffects]

-- | If The Watcher is in the staging area at the end of the combat phase, each player must
-- deal 3 damage to 1 character he controls.
instance HasAbilities TheWatcherNightmare where
  getAbilities (TheWatcherNightmare a) =
    [restricted a 1 (EnemyExists $ EnemyWithId a.id <> EnemyInStagingArea) $ forced $ PhaseEndsWindow When CombatPhase]

instance RunMessage TheWatcherNightmare where
  runMessage msg e@(TheWatcherNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> dealDamageToOneCharacter attrs p 3
      pure e
    _ -> TheWatcherNightmare <$> runMessage msg attrs
