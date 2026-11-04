module LordOfTheRings.Quest.Cards.FortifyTheDefense (fortifyTheDefense) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (getDagnir)

newtype FortifyTheDefense = FortifyTheDefense QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
fortifyTheDefense :: QuestCard FortifyTheDefense
fortifyTheDefense = quest FortifyTheDefense Cards.fortifyTheDefense

-- | Characters do not count their [defense] when defending against Dagnir.
instance HasModifiersFor FortifyTheDefense where
  getModifiersFor (FortifyTheDefense a) = getDagnir >>= traverse_ \d -> modified a (EnemyTarget d) [DefendersDoNotCountDefense]

-- | Response: When Fortify the Defense is defeated, each player chooses: either draw 3
-- cards, or each hero he controls gains 1 resource. The players gain by it, so it is
-- taken, as Seek Shade's is.
instance RunMessage FortifyTheDefense where
  runMessage msg (FortifyTheDefense attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOneM p do
          questionLabeled "fortifyTheDefense.choose"
          causedBy attrs
          labeled "fortifyTheDefense.draw" $ drawCards attrs p 3
          labeled "fortifyTheDefense.resources" $ for_ heroes \h -> placeResources attrs h 1
      FortifyTheDefense <$> runMessage msg attrs
    _ -> FortifyTheDefense <$> runMessage msg attrs
