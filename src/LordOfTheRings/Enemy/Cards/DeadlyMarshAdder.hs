module LordOfTheRings.Enemy.Cards.DeadlyMarshAdder (deadlyMarshAdder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea), placementController)

newtype DeadlyMarshAdder = DeadlyMarshAdder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadlyMarshAdder :: EnemyCard DeadlyMarshAdder
deadlyMarshAdder = enemy DeadlyMarshAdder Cards.deadlyMarshAdder

-- | Forced: After Deadly Marsh Adder engages you, discard a damaged character you control.
-- Forced: After the players advance to a quest stage, return Deadly Marsh Adder to the
-- staging area.
instance HasAbilities DeadlyMarshAdder where
  getAbilities (DeadlyMarshAdder a) =
    mkAbility a 1 (forced $ EnemyEngagedWindow After a.id)
      : [mkAbility a 2 $ forced $ AdvancedToStageWindow After | isJust (placementController a.placement)]

instance RunMessage DeadlyMarshAdder where
  runMessage msg e@(DeadlyMarshAdder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        damaged <- filterM (fmap (> 0) . getDamage) =<< charactersControlledBy pid
        chooseOrRunOneM pid do
          questionLabeled "deadlyMarshAdder.discard"
          causedBy attrs
          targets damaged discard
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      placeEnemy attrs.id StagingArea
      pure e
    _ -> DeadlyMarshAdder <$> runMessage msg attrs
