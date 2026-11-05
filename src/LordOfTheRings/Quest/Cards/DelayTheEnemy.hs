module LordOfTheRings.Quest.Cards.DelayTheEnemy (delayTheEnemy) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DelayTheEnemy = DelayTheEnemy QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Battle and Limit 1 per deck, as its card def says.
delayTheEnemy :: QuestCard DelayTheEnemy
delayTheEnemy = quest DelayTheEnemy Cards.delayTheEnemy

-- | Response: When this stage is defeated, each player may choose and discard a
-- non-unique enemy engaged with him.
instance RunMessage DelayTheEnemy where
  runMessage msg (DelayTheEnemy attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \pid -> do
        enemies <- select (EnemyEngagedWith (PlayerWithId pid) <> not_ UniqueEnemy)
        unless (null enemies) $ chooseOneM pid do
          questionLabeled "delayTheEnemy.discard"
          causedBy attrs
          targets enemies (discard attrs)
          labeled "pass" (pure ())
      DelayTheEnemy <$> runMessage msg attrs
    _ -> DelayTheEnemy <$> runMessage msg attrs
