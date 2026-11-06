module LordOfTheRings.Quest.Cards.SurpriseAttackNibinDum (surpriseAttackNibinDum) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SurpriseAttackNibinDum = SurpriseAttackNibinDum QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

surpriseAttackNibinDum :: QuestCard SurpriseAttackNibinDum
surpriseAttackNibinDum = quest SurpriseAttackNibinDum Cards.surpriseAttackNibinDum

instance HasModifiersFor SurpriseAttackNibinDum where
  getModifiersFor (SurpriseAttackNibinDum a) = when a.flipped do
    enemies <- select AnyEnemy
    modifySelf a [QuestPointModifier (4 * length enemies)]
    phase <- getsGame (.phase)
    when (phase == EncounterPhase) $ for_ enemies \eid -> modified a eid [SetEngagementCost 0]

instance RunMessage SurpriseAttackNibinDum where
  runMessage msg q@(SurpriseAttackNibinDum attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SurpriseAttackNibinDum <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.goblinChieftain)
      players <- getRemainingPlayers
      searchDifferentEncounterCards players attrs (CardWithType EnemyType) True
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      attrs' <- runMessage msg attrs
      checkProgress attrs'
      pure $ SurpriseAttackNibinDum attrs'
    CheckStage qid | qid == attrs.id -> checkProgress attrs >> pure q
    -- Destroy first opens its When window; its DoStep removes the enemy afterward.
    DoStep 1 (Destroy _ (EnemyTarget _)) -> checkAfter
    SetAsideEnemy _ -> checkAfter
    EnemyBecomesHero _ -> checkAfter
    Flip (EnemyTarget _) -> checkAfter
    PlaceEnemy _ _ -> checkAfter
    _ | isJust (removedEnemy msg) -> checkAfter
    _ -> SurpriseAttackNibinDum <$> runMessage msg attrs
   where
    -- The printed zero becomes four per enemy; losing an enemy can complete this stage
    -- without any further questing, so check after that enemy has left play.
    checkAfter = do
      push $ CheckStage attrs.id
      SurpriseAttackNibinDum <$> runMessage msg attrs
    checkProgress a = do
      points <- getQuestPoints a
      when (a.flipped && a.progress >= points) $ completeQuest a.id
