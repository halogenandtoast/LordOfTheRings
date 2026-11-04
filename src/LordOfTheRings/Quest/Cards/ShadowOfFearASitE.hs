module LordOfTheRings.Quest.Cards.ShadowOfFearASitE (shadowOfFearASitE) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ShadowOfFearASitE = ShadowOfFearASitE QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
shadowOfFearASitE :: QuestCard ShadowOfFearASitE
shadowOfFearASitE = quest ShadowOfFearASitE Cards.shadowOfFearASitE

-- | Surge.
-- Forced: After an enemy engages a player, that player raises his threat by 1.
instance HasAbilities ShadowOfFearASitE where
  getAbilities (ShadowOfFearASitE a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After]

-- | Response: When Shadow of Fear is defeated, reduce each player's threat by 3. The players
-- gain by it, so it is taken.
instance RunMessage ShadowOfFearASitE where
  runMessage msg q@(ShadowOfFearASitE attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> encounterRaisesThreat pid 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> lowerThreatBy attrs p 3
      ShadowOfFearASitE <$> runMessage msg attrs
    _ -> ShadowOfFearASitE <$> runMessage msg attrs
