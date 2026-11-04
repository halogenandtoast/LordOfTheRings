module LordOfTheRings.Location.Cards.OverhangingRock (overhangingRock) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype OverhangingRock = OverhangingRock LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overhangingRock :: LocationCard OverhangingRock
overhangingRock = location OverhangingRock Cards.overhangingRock

{- | While Overhanging Rock is the active location, it gains: "Action: Spend 1 Baggins
resource to look at the top 2 cards of your deck. Add 1 of those to your hand and discard
the other."

Any player may trigger it, but only Bilbo Baggins' controller has Baggins resources to
spend.
-}
instance HasAbilities OverhangingRock where
  getAbilities (OverhangingRock a) =
    [anyPlayer $ mkAbility a 1 $ ActionAbility (SpendResourcesCost Baggins 1) | a.placement == ActiveLocation]

instance RunMessage OverhangingRock where
  runMessage msg l@(OverhangingRock a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      takeOneOfTopDiscardRest pid a "overhangingRock.take" 2
      pure l
    _ -> OverhangingRock <$> runMessage msg a
