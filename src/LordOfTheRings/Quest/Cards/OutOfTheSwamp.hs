module LordOfTheRings.Quest.Cards.OutOfTheSwamp (outOfTheSwamp) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (addMarshDweller, getMarshDweller)

newtype OutOfTheSwamp = OutOfTheSwamp QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

outOfTheSwamp :: QuestCard OutOfTheSwamp
outOfTheSwamp = quest OutOfTheSwamp Cards.outOfTheSwamp

-- | Forced: After the last time counter is removed from this stage, Ancient Marsh-dweller
-- makes an attack against each player in order. Place 2 time counters on this stage.
instance HasAbilities OutOfTheSwamp where
  getAbilities (OutOfTheSwamp a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | When Revealed: Add Ancient Marsh-dweller to the staging area (from out of play or
-- engaged with a player) and heal all damage on it. Then Ancient Marsh-dweller makes an
-- attack against each player in player order.
-- When Ancient Marsh-dweller is defeated, the players win the game.
instance RunMessage OutOfTheSwamp where
  runMessage msg q@(OutOfTheSwamp attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      OutOfTheSwamp <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addMarshDweller
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      attackEachPlayer
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      attackEachPlayer
      placeTimeCounters attrs attrs 2
      pure q
    Destroy (EnemyTarget eid) -> do
      whenM ((== Just eid) <$> getMarshDweller) winGame
      OutOfTheSwamp <$> runMessage msg attrs
    _ -> OutOfTheSwamp <$> runMessage msg attrs
   where
    attackEachPlayer = getMarshDweller >>= traverse_ \eid -> eachPlayer \p -> immediateAttack p eid
