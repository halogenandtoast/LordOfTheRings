module LordOfTheRings.Enemy.Cards.TheThingInTheDepths (theThingInTheDepths) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Trait (Trait (Creature))

newtype TheThingInTheDepths = TheThingInTheDepths EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theThingInTheDepths :: EnemyCard TheThingInTheDepths
theThingInTheDepths = enemy TheThingInTheDepths Cards.theThingInTheDepths

-- | Immune to player card effects. Cannot be engaged: it stays in the staging area, no
-- player engaging it nor attacking it, and attacks only as Eye of the Beast and Lashing
-- Tentacles make it.
-- The Thing in the Depths gets +3 hit points per player.
instance HasModifiersFor TheThingInTheDepths where
  getModifiersFor (TheThingInTheDepths a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer, CannotBeAttacked, HitPointModifier (3 * players)]

-- | Forced: When a Creature enemy is destroyed, deal damage to The Thing in the Depths
-- equal to that enemy's threat.
instance RunMessage TheThingInTheDepths where
  runMessage msg (TheThingInTheDepths attrs) = case msg of
    CheckWindows ws -> do
      for_ [eid | Window When (EnemyDefeated eid) <- ws, eid /= attrs.id] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Creature) do
          threat <- getEnemyThreat eid
          when (threat > 0) $ dealDamage attrs attrs threat
      TheThingInTheDepths <$> runMessage msg attrs
    _ -> TheThingInTheDepths <$> runMessage msg attrs
