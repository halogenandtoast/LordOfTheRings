module LordOfTheRings.Enemy.Cards.BellachsHunter (bellachsHunter) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (canRemoveQuestTimeCounter)

newtype BellachsHunter = BellachsHunter EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

bellachsHunter :: EnemyCard BellachsHunter
bellachsHunter = enemy BellachsHunter Cards.bellachsHunter

-- | While a player controls 5 or more allies, Bellach's Hunter gets -20 engagement cost.
instance HasModifiersFor BellachsHunter where
  getModifiersFor (BellachsHunter a) = do
    players <- getRemainingPlayers
    fiveAllies <- anyM (\p -> (>= 5) . length <$> select (AllyControlledBy $ PlayerWithId p)) players
    when fiveAllies $ modifySelf a [EngagementCostModifier (-20)]

-- | Forced: When Bellach's Hunter engages a player, that player must either remove all time
-- counters from the current quest or deal 5 damage to a hero he controls.
instance HasAbilities BellachsHunter where
  getAbilities (BellachsHunter a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage BellachsHunter where
  runMessage msg e@(BellachsHunter attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        removable <- canRemoveQuestTimeCounter
        quest <- getCurrentQuest
        counters <- maybe (pure 0) getTimeCounters quest
        chooseOrRunOneM pid do
          questionLabeled "bellachsHunter.choose"
          causedBy attrs
          when removable $ labeled "bellachsHunter.removeTimeCounters" $ removeTimeCountersFromCurrentQuest attrs counters
          labeled "bellachsHunter.damage" $ dealDamageToOneHero attrs pid 5
      pure e
    _ -> BellachsHunter <$> runMessage msg attrs
