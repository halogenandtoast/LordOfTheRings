module LordOfTheRings.Quest.Cards.FireAndWater (fireAndWater) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies, lakeTownObjectives)

newtype FireAndWater = FireAndWater QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fireAndWater :: QuestCard FireAndWater
fireAndWater = quest FireAndWater Cards.fireAndWater

instance HasModifiersFor FireAndWater where
  getModifiersFor (FireAndWater a) = when a.flipped do
    enemies <- smaugEnemies
    for_ enemies \eid -> modified a eid [MaxDamage 12]
    modified a GameTarget [QuestProgressCanceledBy $ toSource a]

instance RunMessage FireAndWater where
  runMessage msg q@(FireAndWater a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      -- Returning from stage 3 reinstates "cannot have more than 12 damage".
      enemies <- smaugEnemies
      for_ enemies \eid -> do
        damage <- getDamage (EnemyTarget eid)
        when (damage > 12) $ removeTokens a eid Damage (damage - 12)
      FireAndWater <$> runMessage msg a
    ProgressCanceled (isSource a -> True) n | a.flipped && n > 0 -> do
      lead <- getLead
      towns <- lakeTownObjectives
      -- Only the overflow from questing reaches this replacement. Direct progress
      -- effects and progress absorbed by active locations retain their normal path.
      chooseAmountM lead "lakeTown.removeDamage" a 0 n \spent -> do
        for_ towns \oid -> removeTokens a oid Damage spent
        when (n > spent) do
          phaseModifier GameSource a ProgressFromQuestingThisPhase
          placeProgress GameSource a (n - spent)
      pure q
    _ -> FireAndWater <$> runMessage msg a
