module LordOfTheRings.Ally.Cards.RohirrimScout (rohirrimScout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype RohirrimScout = RohirrimScout AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rohirrimScout :: AllyCard RohirrimScout
rohirrimScout = ally RohirrimScout Cards.rohirrimScout

nonUniqueStaged :: EnemyMatcher
nonUniqueStaged = EnemyInStagingArea <> not_ UniqueEnemy

-- | Action: Discard Rohirrim Scout to choose a non-unique enemy in the staging area. That
-- enemy does not make engagement checks against you this round.
instance HasAbilities RohirrimScout where
  getAbilities (RohirrimScout a) =
    [restricted a 1 (exists nonUniqueStaged) $ ActionAbility Free | Just _ <- [placementController a.placement]]

instance RunMessage RohirrimScout where
  runMessage msg s@(RohirrimScout a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a a
      enemies <- select nonUniqueStaged
      chooseOrRunOneM pid do
        questionLabeled "rohirrimScout.chooseEnemy"
        causedBy a
        targets enemies \eid -> roundModifier a eid (NoEngagementChecksAgainst pid)
      pure s
    _ -> RohirrimScout <$> runMessage msg a
