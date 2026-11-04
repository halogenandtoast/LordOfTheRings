module LordOfTheRings.Quest.Cards.UnderTheMercilessSun (underTheMercilessSun) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (increaseTemperature, temperatureX)

newtype UnderTheMercilessSun = UnderTheMercilessSun QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underTheMercilessSun :: QuestCard UnderTheMercilessSun
underTheMercilessSun = quest UnderTheMercilessSun Cards.underTheMercilessSun

-- | Forced: At the end of the round, increase the temperature by 4. Then, each player
-- assigns X damage among characters he controls, where X is the tens digit of the
-- temperature. If the temperature is 60 or higher, the players lose the game (the
-- scenario checks it as it moves).
instance HasAbilities UnderTheMercilessSun where
  getAbilities (UnderTheMercilessSun a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

-- | When Revealed: Increase the temperature by 2. Shuffle the encounter discard pile into
-- the encounter deck and reveal 1 encounter card per player.
-- X is read once the temperature has gone up, as a step of its own.
instance RunMessage UnderTheMercilessSun where
  runMessage msg q@(UnderTheMercilessSun attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      UnderTheMercilessSun <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      increaseTemperature 2
      shuffleEncounterDiscardIntoDeck AnyCard
      players <- length <$> getRemainingPlayers
      revealEncounterCards players
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      increaseTemperature 4
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      x <- temperatureX
      when (x > 0) $ eachPlayer \p -> dealDamageDivided attrs p x
      pure q
    _ -> UnderTheMercilessSun <$> runMessage msg attrs
