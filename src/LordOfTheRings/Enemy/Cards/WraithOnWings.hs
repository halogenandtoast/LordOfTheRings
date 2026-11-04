module LordOfTheRings.Enemy.Cards.WraithOnWings (wraithOnWings) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Morgul))

newtype WraithOnWings = WraithOnWings EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

wraithOnWings :: EnemyCard WraithOnWings
wraithOnWings = enemy WraithOnWings Cards.wraithOnWings

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

-- | Cannot have non-Morgul attachments. Wraith on Wings gets +1 threat and -1 engagement
-- cost for each resource on it.
instance HasModifiersFor WraithOnWings where
  getModifiersFor (WraithOnWings a) =
    modifySelf a $ CannotHaveAttachmentsWithoutTrait Morgul : [m | let r = resources a, r > 0, m <- [ThreatModifier r, EngagementCostModifier (-r)]]

-- | Forced: At the beginning of the quest phase, place 1 resource on Wraith on Wings. (Limit
-- 5 resources.)
instance HasAbilities WraithOnWings where
  getAbilities (WraithOnWings a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | resources a < 5]

instance RunMessage WraithOnWings where
  runMessage msg e@(WraithOnWings attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure e
    _ -> WraithOnWings <$> runMessage msg attrs
