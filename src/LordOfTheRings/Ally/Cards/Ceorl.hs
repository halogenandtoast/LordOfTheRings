module LordOfTheRings.Ally.Cards.Ceorl (ceorl) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype Ceorl = Ceorl AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ceorl :: AllyCard Ceorl
ceorl = ally Ceorl Cards.ceorl

-- | Action: Spend 1 spirit resource to ready Ceorl. Then, you may give control of Ceorl to
-- another player. Any player may trigger this action. (Limit once per phase.)
instance HasAbilities Ceorl where
  getAbilities (Ceorl a) =
    [anyPlayer $ limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ ActionAbility (SpendResourcesCost Spirit 1)]

instance RunMessage Ceorl where
  runMessage msg s@(Ceorl a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      ready a
      others <- filter ((/= placementController a.placement) . Just) <$> getRemainingPlayers
      unless (null others) $ chooseOneM pid do
        questionLabeled "ceorl.giveControl"
        causedBy a
        targets others (takeControl a.id)
        labeled "ceorl.keep" (pure ())
      pure s
    _ -> Ceorl <$> runMessage msg a
