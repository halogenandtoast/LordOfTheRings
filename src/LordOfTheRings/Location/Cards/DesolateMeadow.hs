module LordOfTheRings.Location.Cards.DesolateMeadow (desolateMeadow) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Creature))

newtype DesolateMeadow = DesolateMeadow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

desolateMeadow :: LocationCard DesolateMeadow
desolateMeadow = location DesolateMeadow Cards.desolateMeadow

-- | While Desolate Meadow is in the staging area, it gains: "Forced: After any number of
-- time counters are removed from the current quest, heal 2 damage on each Creature enemy
-- in play."
instance HasAbilities DesolateMeadow where
  getAbilities (DesolateMeadow a) = [mkAbility a 1 $ forced $ TimeCountersRemovedFromQuestWindow After | a.placement == StagingArea]

-- | Shadow: If this attack destroys a character, remove all progress from the current
-- quest.
instance RunMessage DesolateMeadow where
  runMessage msg l@(DesolateMeadow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      creatures <- select (EnemyWithTrait Creature)
      for_ creatures \e -> healDamage a e 2
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      current <- getCurrentQuest
      quests <- getsGame (filter ((== current) . Just . toTarget) . toList . (.entities.quests))
      for_ quests \q -> when (q.progress > 0) $ removeTokens a q Progress q.progress
      pure l
    _ -> DesolateMeadow <$> runMessage msg a
