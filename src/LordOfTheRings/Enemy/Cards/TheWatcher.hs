module LordOfTheRings.Enemy.Cards.TheWatcher (theWatcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Tentacle))

newtype TheWatcher = TheWatcher EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWatcher :: EnemyCard TheWatcher
theWatcher = enemy TheWatcher Cards.theWatcher

-- | Regenerate 2.
-- While there is another Tentacle enemy in play, The Watcher cannot be optionally engaged.
instance HasModifiersFor TheWatcher where
  getModifiersFor (TheWatcher a) = do
    others <- selectAny (EnemyWithTrait Tentacle <> not_ (EnemyWithId a.id))
    when others $ modified a a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | If The Watcher is in the staging area at the end of the combat phase, each player must
-- deal 3 damage to 1 character he controls.
instance HasAbilities TheWatcher where
  getAbilities (TheWatcher a) =
    [restricted a 1 (EnemyExists $ EnemyWithId a.id <> EnemyInStagingArea) $ forced $ PhaseEndsWindow When CombatPhase]

instance RunMessage TheWatcher where
  runMessage msg e@(TheWatcher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> dealDamageToOneCharacter attrs p 3
      pure e
    _ -> TheWatcher <$> runMessage msg attrs
