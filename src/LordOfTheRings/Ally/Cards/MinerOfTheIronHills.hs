module LordOfTheRings.Ally.Cards.MinerOfTheIronHills (minerOfTheIronHills) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype MinerOfTheIronHills = MinerOfTheIronHills AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

minerOfTheIronHills :: AllyCard MinerOfTheIronHills
minerOfTheIronHills = ally MinerOfTheIronHills Cards.minerOfTheIronHills

-- | Response: After Miner of the Iron Hills enters play, choose and discard 1 Condition
-- attachment from play.
instance HasAbilities MinerOfTheIronHills where
  getAbilities (MinerOfTheIronHills a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage MinerOfTheIronHills where
  runMessage msg s@(MinerOfTheIronHills a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      conditions <- getConditionAttachments
      chooseOneM pid do
        questionLabeled "minerOfTheIronHills.discardCondition"
        causedBy a
        targets conditions discard
      pure s
    _ -> MinerOfTheIronHills <$> runMessage msg a
