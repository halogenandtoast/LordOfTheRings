module LordOfTheRings.Ally.Cards.BeorningBeekeeper (beorningBeekeeper) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype BeorningBeekeeper = BeorningBeekeeper AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beorningBeekeeper :: AllyCard BeorningBeekeeper
beorningBeekeeper = ally BeorningBeekeeper Cards.beorningBeekeeper

-- | Action: Discard Beorning Beekeeper from play to deal 1 damage to each enemy in the
-- staging area.
instance HasAbilities BeorningBeekeeper where
  getAbilities (BeorningBeekeeper a) = [mkAbility a 1 $ ActionAbility Free]

instance RunMessage BeorningBeekeeper where
  runMessage msg s@(BeorningBeekeeper a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      enemies <- select EnemyInStagingArea
      for_ enemies \e -> dealDamage a e 1
      pure s
    _ -> BeorningBeekeeper <$> runMessage msg a
