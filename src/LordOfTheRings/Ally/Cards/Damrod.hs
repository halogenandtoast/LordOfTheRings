module LordOfTheRings.Ally.Cards.Damrod (damrod) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Damrod = Damrod AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

damrod :: AllyCard Damrod
damrod = ally Damrod Cards.damrod

-- | Action: Discard Damrod from play to lower your threat by 1 for each enemy in the
-- staging area.
instance HasAbilities Damrod where
  getAbilities (Damrod a) = [mkAbility a 1 $ ActionAbility Free]

instance RunMessage Damrod where
  runMessage msg s@(Damrod a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      n <- length <$> select EnemyInStagingArea
      discard a
      lowerThreatBy a pid n
      pure s
    _ -> Damrod <$> runMessage msg a
