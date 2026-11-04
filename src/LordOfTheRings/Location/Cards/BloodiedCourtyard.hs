module LordOfTheRings.Location.Cards.BloodiedCourtyard (bloodiedCourtyard) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype BloodiedCourtyard = BloodiedCourtyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bloodiedCourtyard :: LocationCard BloodiedCourtyard
bloodiedCourtyard = location BloodiedCourtyard Cards.bloodiedCourtyard

-- | While Bloodied Courtyard is the active location, it gains: "Forced: After an enemy
-- enters the staging area, deal 1 damage to it."
instance HasAbilities BloodiedCourtyard where
  getAbilities (BloodiedCourtyard a) = [mkAbility a 1 $ forced $ EnemyEntersStagingAreaWindow After | a.placement == ActiveLocation]

-- | Travel: Raise each player's threat by 1 to travel here.
instance RunMessage BloodiedCourtyard where
  runMessage msg l@(BloodiedCourtyard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [t | Window _ (EntersStagingArea t@(EnemyTarget _)) <- ws] \t -> dealDamage a t 1
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> raiseThreat p 1
      BloodiedCourtyard <$> runMessage msg a
    _ -> BloodiedCourtyard <$> runMessage msg a
