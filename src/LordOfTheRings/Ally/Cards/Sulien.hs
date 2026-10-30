module LordOfTheRings.Ally.Cards.Sulien (sulien) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype Sulien = Sulien AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sulien :: AllyCard Sulien
sulien = ally Sulien Cards.sulien

-- | Action: Spend 1 lore resource to exhaust Súlien. Then, each location in the staging
-- area gets -1 threat until the end of the phase. Any player may trigger this action.
instance HasAbilities Sulien where
  getAbilities (Sulien a) =
    [anyPlayer $ mkAbility a 1 $ ActionAbility (Costs [SpendResourcesCost Lore 1, exhaustCost a])]

-- | A location entering the staging area later in the phase gets it too (FAQ 2.43).
instance RunMessage Sulien where
  runMessage msg s@(Sulien a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifier a GameTarget (EachLocationInStagingArea (ThreatModifier (-1)))
      pure s
    _ -> Sulien <$> runMessage msg a
