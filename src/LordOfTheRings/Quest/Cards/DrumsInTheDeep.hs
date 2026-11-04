module LordOfTheRings.Quest.Cards.DrumsInTheDeep (drumsInTheDeep) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype DrumsInTheDeep = DrumsInTheDeep QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

drumsInTheDeep :: QuestCard DrumsInTheDeep
drumsInTheDeep = quest DrumsInTheDeep Cards.drumsInTheDeep

-- | The players cannot advance unless there are X resource tokens on this stage. X is the
-- number of players times 3.
instance HasModifiersFor DrumsInTheDeep where
  getModifiersFor (DrumsInTheDeep a) = when a.flipped do
    players <- length <$> getRemainingPlayers
    when (countTokens Resource a.questTokens < players * 3) $ modifySelf a [CannotBeDefeated]

{- | If there are no enemies in the staging area at the beginning of the quest phase, the
first player reveals an additional encounter card during the staging step (the extra card
of the step goes to the first player).
Forced: After an enemy is defeated, place 1 resource token on this stage.
-}
instance HasAbilities DrumsInTheDeep where
  getAbilities (DrumsInTheDeep a) =
    [ ability
    | a.flipped
    , ability <- [mkAbility a 1 $ silent $ PhaseBeginsWindow After QuestPhase, mkAbility a 2 $ forced $ EnemyDefeatedWindow After]
    ]

-- | When Revealed: Add Chamber of Mazarbul to the staging area.
instance RunMessage DrumsInTheDeep where
  runMessage msg q@(DrumsInTheDeep attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DrumsInTheDeep <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.chamberOfMazarbul)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      staged <- selectAny EnemyInStagingArea
      unless staged $ phaseModifier attrs GameTarget (AdditionalStagingReveals 1)
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      let defeated = length [() | Window _ (EnemyDefeated _) <- ws]
      when (defeated > 0) $ placeResources attrs attrs defeated
      pure q
    _ -> DrumsInTheDeep <$> runMessage msg attrs
