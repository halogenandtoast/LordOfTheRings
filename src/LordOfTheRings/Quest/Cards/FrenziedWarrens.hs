module LordOfTheRings.Quest.Cards.FrenziedWarrens (frenziedWarrens) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype FrenziedWarrens = FrenziedWarrens QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frenziedWarrens :: QuestCard FrenziedWarrens
frenziedWarrens = quest FrenziedWarrens Cards.frenziedWarrens

-- | Forced: After characters are committed to the quest, discard the top 5 cards of the
-- encounter deck. Add a random enemy discarded this way to the staging area.
instance HasAbilities FrenziedWarrens where
  getAbilities (FrenziedWarrens a) =
    exploringGundabad a <> bypass a <> [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]

-- | Forced: When this stage is defeated, reduce each player's threat by 5.
instance RunMessage FrenziedWarrens where
  runMessage msg q@(FrenziedWarrens attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 5
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let enemies = filter (cardMatches $ CardWithType EnemyType) cards
      unless (null enemies) $ addToStagingFromDiscard =<< uniform enemies
      pure q
    _ -> FrenziedWarrens <$> runExploringGundabad defeated msg attrs
   where
    defeated = eachPlayer \p -> lowerThreatBy attrs p 5
