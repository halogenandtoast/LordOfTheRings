module LordOfTheRings.Quest.Cards.TheDefenseOfRavenhill (theDefenseOfRavenhill) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (runBattleStage)

newtype TheDefenseOfRavenhill = TheDefenseOfRavenhill QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Siege, on its card def.
theDefenseOfRavenhill :: QuestCard TheDefenseOfRavenhill
theDefenseOfRavenhill = quest TheDefenseOfRavenhill Cards.theDefenseOfRavenhill

-- | Forced: If there is no progress on this stage at the end of the round, each player must
-- discard all resources from each hero's resource pool.
instance HasAbilities TheDefenseOfRavenhill where
  getAbilities (TheDefenseOfRavenhill a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped, a.progress == 0]

-- | Players cannot advance from this stage unless stage 2 and stage 3 are complete (see
-- 'runBattleStage').
instance RunMessage TheDefenseOfRavenhill where
  runMessage msg q@(TheDefenseOfRavenhill attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeAllResourcesFromEachHero attrs
      pure q
    _ -> TheDefenseOfRavenhill <$> runBattleStage msg attrs
