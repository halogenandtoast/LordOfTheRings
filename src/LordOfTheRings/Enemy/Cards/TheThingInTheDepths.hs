module LordOfTheRings.Enemy.Cards.TheThingInTheDepths (theThingInTheDepths) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
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
    campaign <- isJust <$> getCampaignLog
    modifySelf a $ [ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer, CannotBeAttacked, HitPointModifier (3 * players)]
      -- its Campaign Mode only side: 3 threat, 6 attack, 4 hit points
      <> [m | campaign, m <- [ThreatModifier 1, AttackModifier 1, HitPointModifier (-4)]]

-- | Forced: When a Creature enemy is destroyed, deal damage to The Thing in the Depths
-- equal to that enemy's threat.
instance RunMessage TheThingInTheDepths where
  runMessage msg (TheThingInTheDepths attrs) = case msg of
    CheckWindows ws -> do
      for_ [eid | Window When (EnemyDefeated eid) <- ws, eid /= attrs.id] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Creature) do
          -- its Campaign Mode only side deals 2
          campaign <- isJust <$> getCampaignLog
          threat <- if campaign then pure 2 else getEnemyThreat eid
          when (threat > 0) $ dealDamage attrs attrs threat
      TheThingInTheDepths <$> runMessage msg attrs
    _ -> TheThingInTheDepths <$> runMessage msg attrs
