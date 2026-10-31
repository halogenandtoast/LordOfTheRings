module LordOfTheRings.Quest.Cards.ForcedOffTrack (forcedOffTrack) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Warg))

newtype ForcedOffTrack = ForcedOffTrack QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
forcedOffTrack :: QuestCard ForcedOffTrack
forcedOffTrack = quest ForcedOffTrack Cards.forcedOffTrack

-- | Forced: At the end of the round, remove X progress from the main quest, where X is the
-- number of Warg enemies in play.
instance HasAbilities ForcedOffTrack where
  getAbilities (ForcedOffTrack a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Response: When this quest is defeated, discard a non-unique Warg enemy in the staging
-- area. The players gain by it, so it is taken.
instance RunMessage ForcedOffTrack where
  runMessage msg q@(ForcedOffTrack attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- length <$> select (EnemyWithTrait Warg)
      mains <- getMainQuests
      when (x > 0) $ for_ mains \m -> removeTokens attrs m Progress x
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      wargs <- select (EnemyInStagingArea <> EnemyWithTrait Warg <> not_ UniqueEnemy)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "forcedOffTrack.discard"
        causedBy attrs
        targets wargs discard
      ForcedOffTrack <$> runMessage msg attrs
    _ -> ForcedOffTrack <$> runMessage msg attrs
