{- | What the cards of The City of Ulfast share: City Guard, the X counted from the resources
on the main quest, and the side B text every stage repeats.
-}
module LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (
  cityGuard,
  getCityGuard,
  ulfastX,
  cityGuardStageModifiers,
  cityGuardStageAbilities,
  runCityGuardStage,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner
import LordOfTheRings.Queue
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Resource), countTokens)

cityGuard :: EnemyMatcher
cityGuard = EnemyWithCardCode $ toCardCode Enemies.cityGuard

getCityGuard :: HasGame m => m (Maybe EnemyId)
getCityGuard = selectOne cityGuard

-- | "X is 1 more than the number of resources on the main quest."
ulfastX :: HasGame m => m Int
ulfastX = (+ 1) <$> getMainQuestResources

-- | Each stage's side B: "City Guard cannot have more than N damage, and it gets -5
-- engagement cost for each resource here."
cityGuardStageModifiers :: HasModifiersM m => QuestAttrs -> Int -> m ()
cityGuardStageModifiers a n = when a.flipped do
  let resources = countTokens Resource a.questTokens
  getCityGuard >>= traverse_ \g ->
    modified a (EnemyTarget g) $ MaxDamage n : [EngagementCostModifier (-5 * resources) | resources > 0]

-- | Each stage's side B: "Forced: At the end of the round, place 1 resource here."
cityGuardStageAbilities :: QuestAttrs -> [Ability]
cityGuardStageAbilities a = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

-- | The stage's Forced effect, else the stage as any other.
runCityGuardStage :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runCityGuardStage msg attrs = case msg of
  UseCardAbility _ (isSource attrs -> True) 1 _ -> do
    placeResources attrs attrs 1
    pure attrs
  _ -> runMessage msg attrs
