module LordOfTheRings.Enemy.Cards.GoblinChieftain (goblinChieftain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Card (toCardCode)

newtype GoblinChieftain = GoblinChieftain EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinChieftain :: EnemyCard GoblinChieftain
goblinChieftain = enemy GoblinChieftain Cards.goblinChieftain

instance HasModifiersFor GoblinChieftain where
  getModifiersFor (GoblinChieftain a) = do
    n <- getMainStage
    quests <- getMainQuestCards
    let oathkeepers = any (\quest -> toCardCode quest == "dark-of-mirkwood-007" && quest.progress >= 8) quests
    modifySelf a $ [ThreatModifier n, AttackModifier n, DefenseModifier n, CannotHaveAttachments] <> [CannotTakeDamage | not oathkeepers]

instance HasAbilities GoblinChieftain where
  getAbilities (GoblinChieftain a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage GoblinChieftain where
  runMessage msg e@(GoblinChieftain a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      n <- getMainStage
      current <- getCurrentQuest
      quests <- getMainQuestCards
      let enough = any (\q -> Just (toTarget q) == current && q.progress >= n) quests
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "goblinChieftain.choose"
          causedBy a
          when enough $ labeled "goblinChieftain.progress" $ removeProgressFromQuest a n
          labeled "goblinChieftain.return" $ placeEnemy a.id StagingArea
      pure e
    _ -> GoblinChieftain <$> runMessage msg a
