module LordOfTheRings.Ally.Cards.FarmerMaggot (farmerMaggot) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Helpers.Staging (getEngagementCost)

newtype FarmerMaggot = FarmerMaggot AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

farmerMaggot :: AllyCard FarmerMaggot
farmerMaggot = ally FarmerMaggot Cards.farmerMaggot

-- | Response: After Farmer Maggot enters play, deal 1 damage to an enemy engaged with you.
-- (Deal 2 damage instead if that enemy's engagement cost is higher than your threat.)
instance HasAbilities FarmerMaggot where
  getAbilities (FarmerMaggot a) =
    [ restricted a 1 (exists $ EnemyEngagedWith (PlayerWithId pid)) $ response $ entersPlay a
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage FarmerMaggot where
  runMessage msg s@(FarmerMaggot a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      threat <- getThreat pid
      chooseOrRunOneM pid do
        questionLabeled "farmerMaggot.damage"
        causedBy a
        targets enemies \eid -> do
          cost <- getEngagementCost eid
          dealDamage a eid (if cost > threat then 2 else 1)
      pure s
    _ -> FarmerMaggot <$> runMessage msg a
