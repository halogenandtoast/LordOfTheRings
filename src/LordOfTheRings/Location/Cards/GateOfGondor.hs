module LordOfTheRings.Location.Cards.GateOfGondor (gateOfGondor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype GateOfGondor = GateOfGondor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

gateOfGondor :: LocationCard GateOfGondor
gateOfGondor = location GateOfGondor Cards.gateOfGondor

-- | Immune to player card effects. While Gate of Gondor is the active location, each enemy
-- in play gets -30 engagement cost.
instance HasModifiersFor GateOfGondor where
  getModifiersFor (GateOfGondor a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    when (a.placement == ActiveLocation) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [EngagementCostModifier (-30)]

-- | Forced: When Gate of Gondor is explored, return each engaged enemy to the staging area.
instance HasAbilities GateOfGondor where
  getAbilities (GateOfGondor a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage GateOfGondor where
  runMessage msg l@(GateOfGondor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnEngagedEnemiesToStaging
      pure l
    _ -> GateOfGondor <$> runMessage msg a
