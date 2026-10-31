module LordOfTheRings.Enemy.Cards.ThaurdirsHunter (thaurdirsHunter) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gameEntities)

newtype ThaurdirsHunter = ThaurdirsHunter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thaurdirsHunter :: EnemyCard ThaurdirsHunter
thaurdirsHunter = enemy ThaurdirsHunter Cards.thaurdirsHunter

-- | Forced: When Thaurdir's Hunter attacks, either remove 1 time counter from the main
-- quest, or treat this attack as undefended.
instance HasAbilities ThaurdirsHunter where
  getAbilities (ThaurdirsHunter a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: If this attack is undefended, either remove 1 time counter or 5 progress from a
-- quest.
instance RunMessage ThaurdirsHunter where
  runMessage msg e@(ThaurdirsHunter attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      mains <- filterM canRemoveTimeCounter =<< getMainQuests
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "thaurdirsHunter.choose"
          causedBy attrs
          for_ (take 1 mains) \q -> labeled "thaurdirsHunter.timeCounter" $ removeTimeCounters attrs q 1
          labeled "thaurdirsHunter.undefended" $ attackModifier attrs attrs.id CannotBeDefended
      pure e
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId, null defenders -> do
      quests <- getsGame (toList . entitiesQuests . gameEntities)
      timed <- filterM (canRemoveTimeCounter . toTarget) quests
      let source = CardIdSource cid
      chooseOrRunOneM pid do
        questionLabeled "thaurdirsHunter.remove"
        causedBy attrs
        for_ timed \q -> labeledWith "thaurdirsHunter.removeTime" (cardVar "card" q) $ removeTimeCounters source q 1
        for_ [q | q <- quests, q.progress > 0] \q ->
          labeledWith "thaurdirsHunter.removeProgress" (cardVar "card" q) $ removeTokens source q Progress (min 5 q.progress)
      pure e
    _ -> ThaurdirsHunter <$> runMessage msg attrs
