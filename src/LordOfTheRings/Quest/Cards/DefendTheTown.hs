module LordOfTheRings.Quest.Cards.DefendTheTown (defendTheTown) where

import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (damageHrogarsHill)

newtype DefendTheTown = DefendTheTown QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
defendTheTown :: QuestCard DefendTheTown
defendTheTown = quest DefendTheTown Cards.defendTheTown

-- | While Defend the Town is the current quest, reveal an additional encounter card during
-- the quest phase.
instance HasModifiersFor DefendTheTown where
  getModifiersFor (DefendTheTown a) = do
    current <- getCurrentQuest
    when (current == Just (toTarget a)) $ modified a GameTarget [AdditionalStagingReveals 1]

-- | Forced: At the end of the encounter phase, deal 1 damage to Hrogar's Hill for each
-- enemy in the staging area: Dagnir, who cannot leave it, among them.
instance HasAbilities DefendTheTown where
  getAbilities (DefendTheTown a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase]

instance RunMessage DefendTheTown where
  runMessage msg q@(DefendTheTown attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      enemies <- length <$> select EnemyInStagingArea
      damageHrogarsHill attrs enemies
      pure q
    _ -> DefendTheTown <$> runMessage msg attrs
