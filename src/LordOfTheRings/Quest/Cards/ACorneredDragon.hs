module LordOfTheRings.Quest.Cards.ACorneredDragon (aCorneredDragon) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (coldDrake, dragonSignsInVictoryDisplay, getColdDrake, returnColdDrake)

newtype ACorneredDragon = ACorneredDragon QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

aCorneredDragon :: QuestCard ACorneredDragon
aCorneredDragon = quest ACorneredDragon Cards.aCorneredDragon

-- | Cold-Drake cannot take more than X damage each round, where X is the number of Dragon
-- Signs in the victory display.
-- This stage cannot be defeated while Cold-Drake is in play.
instance HasModifiersFor ACorneredDragon where
  getModifiersFor (ACorneredDragon a) = when a.flipped do
    x <- dragonSignsInVictoryDisplay
    getColdDrake >>= traverse_ \eid -> do
      modified a eid [DamageLimit x]
      modifySelf a [CannotBeDefeated]

-- | Forced: At the beginning of the quest phase, return Cold-Drake to the staging area. It
-- makes an immediate attack against each player in turn order.
instance HasAbilities ACorneredDragon where
  getAbilities (ACorneredDragon a) = [restricted a 1 (exists coldDrake) $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

{- | When Revealed: Add Cold-Drake to the staging area. It makes an immediate attack against
each player in turn order. (Deal and resolve a shadow card for each attack.)
When Cold-Drake is defeated, the players win the game.
-}
instance RunMessage ACorneredDragon where
  runMessage msg q@(ACorneredDragon attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      ACorneredDragon <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      returnColdDrake
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      attackEachPlayer
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getColdDrake >>= traverse_ \eid -> do
        e <- getEnemy eid
        when (e.placement /= StagingArea) $ placeEnemy eid StagingArea
      attackEachPlayer
      pure q
    Destroy _ (EnemyTarget eid) -> do
      defeated <- selectAny (EnemyWithId eid <> coldDrake)
      when defeated winGame
      ACorneredDragon <$> runMessage msg attrs
    _ -> ACorneredDragon <$> runMessage msg attrs
   where
    attackEachPlayer = getColdDrake >>= traverse_ \eid -> eachPlayer \p -> immediateAttack p eid
