module LordOfTheRings.Quest.Cards.RetrieveUrdugsHorn (retrieveUrdugsHorn) where

import LordOfTheRings.Card (toCard, toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion.Helpers (guardOf, searchAndGuard)

newtype RetrieveUrdugsHorn = RetrieveUrdugsHorn QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

retrieveUrdugsHorn :: QuestCard RetrieveUrdugsHorn
retrieveUrdugsHorn = quest RetrieveUrdugsHorn Cards.retrieveUrdugsHorn

-- | Each enemy with 1 or more guarded attachments gets +2 threat and +2 defense.
-- The Grey Mountain Giant guarding Urdug's Horn cannot take damage unless there is at least
-- 5 progress on this stage.
instance HasModifiersFor RetrieveUrdugsHorn where
  getModifiersFor (RetrieveUrdugsHorn a) = when a.flipped do
    guarding <- select EnemyGuardingACard
    for_ guarding \e -> modified a e [ThreatModifier 2, DefenseModifier 2]
    when (a.progress < 5) do
      guard' <- guardOf Objectives.urdugsHorn
      giants <- getsGame (filter ((== guard') . Just . (.id) . toCard) . toList . (.entities.enemies))
      for_ giants \g -> modified a g [CannotTakeDamage]

-- | This stage cannot be defeated unless the first player controls Urdug with Urdug's Horn
-- attached.
defeatable :: HasGame m => m Bool
defeatable = do
  lead <- getLead
  urdug <- select (AllyWithCardCode (toCardCode Objectives.urdug) <> AllyControlledBy (PlayerWithId lead))
  horns <- getsGame (filter ((== toCardCode Objectives.urdugsHorn) . toCardCode) . toList . (.entities.objectives))
  pure $ or [h.placement == AttachedTo (toTarget u) | h <- horns, u <- urdug]

{- | When Revealed: The first player searches the encounter deck, discard pile, and victory
display for a Grey Mountain Giant, adds it to the staging area, and attaches the set-aside
Urdug's Horn to it as a guarded objective. Shuffle the encounter deck. Each other player
reveals an encounter card.
Forced: When the players defeat this stage, remove it from the game. Advance to a random
stage 2A, if able. Otherwise, advance to stage 3A (the scenario draws it).
-}
instance RunMessage RetrieveUrdugsHorn where
  runMessage msg q@(RetrieveUrdugsHorn attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RetrieveUrdugsHorn <$> runConditionalStage defeatable msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchAndGuard Enemies.greyMountainGiant Objectives.urdugsHorn
      pure q
    RescueCharacter {} -> recheck
    SetFirstPlayer {} -> recheck
    _ -> RetrieveUrdugsHorn <$> runConditionalStage defeatable msg attrs
   where
    recheck = do
      checkStage attrs.id
      RetrieveUrdugsHorn <$> runConditionalStage defeatable msg attrs
