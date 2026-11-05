module LordOfTheRings.Ally.Cards.BeorningGuardian (beorningGuardian) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameAttackDestroyed)

newtype BeorningGuardian = BeorningGuardian AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beorningGuardian :: AllyCard BeorningGuardian
beorningGuardian = ally BeorningGuardian Cards.beorningGuardian

-- | Response: After Beorning Guardian participates in an attack that destroys an enemy,
-- discard Beorning Guardian to place X progress on a location, where X is the destroyed
-- enemy's printed threat.
instance HasAbilities BeorningGuardian where
  getAbilities (BeorningGuardian a) =
    [restricted a 1 (exists AnyLocation) $ response $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)]

instance RunMessage BeorningGuardian where
  runMessage msg s@(BeorningGuardian a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      -- the enemy is gone: its card is read from what the attack destroyed
      let printedThreat d = fromMaybe 0 (cdThreat (toCardDef d.card))
      destroyed <- getsGame gameAttackDestroyed
      let xs =
            [ printedThreat d
            | Window _ (EnemyDestroyedByAttack eid _ _) <- windows
            , d <- take 1 [d | d <- destroyed, d.target == EnemyTarget eid]
            ]
      discard a a
      locations <- select AnyLocation
      chooseOrRunOneM pid do
        questionLabeled "beorningGuardian.location"
        causedBy a
        targets locations \l -> placeProgress a l (sum xs)
      pure s
    _ -> BeorningGuardian <$> runMessage msg a
