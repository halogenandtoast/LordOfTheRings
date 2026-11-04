module LordOfTheRings.Ally.Cards.MablungLore (mablungLore) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MablungLore = MablungLore AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mablungLore :: AllyCard MablungLore
mablungLore = ally MablungLore Cards.mablungLore

-- | Response: After Mablung enters play, choose an enemy to get +5 engagement cost until
-- the end of the round. Then, you may engage that enemy, or return it to the staging area.
instance HasAbilities MablungLore where
  getAbilities (MablungLore a) =
    [restricted a 1 (exists EnemyNotImmuneToPlayerCards) $ response $ entersPlay a]

instance RunMessage MablungLore where
  runMessage msg s@(MablungLore a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select EnemyNotImmuneToPlayerCards
      chooseOrRunOneM pid do
        questionLabeled "mablung.chooseEnemy"
        causedBy a
        targets enemies \eid -> do
          roundModifier a eid (EngagementCostModifier 5)
          staged <- selectAny (EnemyWithId eid <> EnemyInStagingArea)
          engagedWithYou <- selectAny (EnemyWithId eid <> EnemyEngagedWith (PlayerWithId pid))
          chooseOneM pid do
            questionLabeled "mablung.engageOrReturn"
            causedBy a
            unless engagedWithYou $ labeled "mablung.engage" $ engageEnemy pid eid
            unless staged $ labeled "mablung.returnToStaging" $ placeEnemy eid StagingArea
            labeled "mablung.neither" $ pure ()
      pure s
    _ -> MablungLore <$> runMessage msg a
