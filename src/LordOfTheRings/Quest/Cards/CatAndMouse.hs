module LordOfTheRings.Quest.Cards.CatAndMouse (catAndMouse) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype CatAndMouse = CatAndMouse QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
catAndMouse :: QuestCard CatAndMouse
catAndMouse = quest CatAndMouse Cards.catAndMouse

-- | Each enemy in the staging area gets -10 engagement cost.
instance HasModifiersFor CatAndMouse where
  getModifiersFor (CatAndMouse a) = do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [EngagementCostModifier (-10)]

-- | Response: When Cat and Mouse is defeated, each player may discard a non-unique enemy
-- engaged with him.
instance RunMessage CatAndMouse where
  runMessage msg (CatAndMouse attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> do
        enemies <- select (EnemyEngagedWith (PlayerWithId p) <> NotEnemy UniqueEnemy)
        unless (null enemies) $ chooseOneM p do
          questionLabeled "catAndMouse.discard"
          causedBy attrs
          targets enemies (discard attrs)
          labeled "pass" (pure ())
      CatAndMouse <$> runMessage msg attrs
    _ -> CatAndMouse <$> runMessage msg attrs
