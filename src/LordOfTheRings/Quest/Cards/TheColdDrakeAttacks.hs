module LordOfTheRings.Quest.Cards.TheColdDrakeAttacks (theColdDrakeAttacks) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (getColdDrake)

newtype TheColdDrakeAttacks = TheColdDrakeAttacks QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theColdDrakeAttacks :: QuestCard TheColdDrakeAttacks
theColdDrakeAttacks = quest TheColdDrakeAttacks Cards.theColdDrakeAttacks

{- | Ignore the Deep keyword.
Cold-Drake cannot leave the staging area (even if it has damage equal to its hit points),
but it is considered to be engaged with each player: it attacks each of them, dealt a new
shadow card for each attack, as Durin's Bane does, and it is not destroyed, as an
indestructible enemy is not.
No more than 6 damage can be placed on the Cold-Drake each round and it cannot have more
than 18 damage.
This stage cannot be defeated while Cold-Drake has any hit points remaining.
-}
instance HasModifiersFor TheColdDrakeAttacks where
  getModifiersFor (TheColdDrakeAttacks a) = when a.flipped do
    modified a GameTarget [IgnoreDeep]
    getColdDrake >>= traverse_ \eid -> do
      damage <- (.damage) <$> getEnemy eid
      modified
        a
        eid
        [ CannotLeaveStagingArea
        , NoEngagementChecks
        , CannotBeOptionallyEngagedBy AnyPlayer
        , EngagedWithEachPlayer AnyPlayer
        , GainKeyword Indestructible
        , DamageLimit 6
        , DamageLimit (max 0 (18 - damage))
        ]
      hitPoints <- getEnemyHitPoints eid
      when (damage < hitPoints) $ modifySelf a [CannotBeDefeated]

-- | When Revealed: Add the set-aside Cold-Drake to the staging area.
instance RunMessage TheColdDrakeAttacks where
  runMessage msg q@(TheColdDrakeAttacks attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheColdDrakeAttacks <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.coldDrake)
      pure q
    _ -> TheColdDrakeAttacks <$> runMessage msg attrs
