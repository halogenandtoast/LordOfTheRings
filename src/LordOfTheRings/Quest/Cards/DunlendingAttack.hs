module LordOfTheRings.Quest.Cards.DunlendingAttack (dunlendingAttack) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers

newtype DunlendingAttack = DunlendingAttack QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlendingAttack :: QuestCard DunlendingAttack
dunlendingAttack = quest DunlendingAttack Cards.dunlendingAttack

-- | Time 2. Forced: After the last time counter is removed from this stage, each player
-- assigns X damage among characters he controls where X is the number of cards in his
-- hand. Place 2 time counters on this stage.
instance HasAbilities DunlendingAttack where
  getAbilities (DunlendingAttack a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a | a.flipped]

-- | When Revealed: Each player searches the encounter deck and discard pile for 1
-- different Dunland enemy and adds it to the staging area. Shuffle the encounter deck.
instance RunMessage DunlendingAttack where
  runMessage msg q@(DunlendingAttack attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      DunlendingAttack <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchForDifferentDunlandEnemies attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> assignDamageAmongCharacters attrs p =<< handSize p
      placeTimeCounters attrs attrs 2
      pure q
    _ -> DunlendingAttack <$> runMessage msg attrs
