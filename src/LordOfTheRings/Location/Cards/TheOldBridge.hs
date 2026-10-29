module LordOfTheRings.Location.Cards.TheOldBridge (theOldBridge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Trait (Trait (Osgiliath))

newtype TheOldBridge = TheOldBridge LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldBridge :: LocationCard TheOldBridge
theOldBridge = location TheOldBridge Cards.theOldBridge

-- | The players cannot travel here.
instance HasModifiersFor TheOldBridge where
  getModifiersFor (TheOldBridge a) = modified a a [CannotBeTraveledTo]

-- | Planning Action: Spend 2 resources to place 1 progress token on The Old Bridge. (1
-- resource instead if there is another Osgiliath location in the staging area.)
-- Offered under a player's control too: progress may still be placed there, and exploring
-- it gives it to the first player (ruling). The cheaper cost is a second ability, offered
-- only while another Osgiliath location is in the staging area.
-- Forced: After the player who controls The Old Bridge lets an attack go undefended,
-- return The Old Bridge to the staging area.
instance HasAbilities TheOldBridge where
  getAbilities (TheOldBridge a) =
    [ anyPlayer $ restricted a 1 (DuringPhase PlanningPhase <> NotCriterion (exists $ anotherOsgiliath a)) $ ActionAbility (SpendResourcesCost Neutral 2)
    | a.placement /= Unplaced
    ]
      <> returnsWhenUndefended a 2
      <> [ anyPlayer $ restricted a 3 (DuringPhase PlanningPhase <> exists (anotherOsgiliath a)) $ ActionAbility (SpendResourcesCost Neutral 1)
         | a.placement /= Unplaced
         ]

anotherOsgiliath :: LocationAttrs -> LocationMatcher
anotherOsgiliath a = LocationInStagingArea <> LocationWithTrait Osgiliath <> not_ (LocationWithId a.id)

instance RunMessage TheOldBridge where
  runMessage msg l@(TheOldBridge a) = case msg of
    UseCardAbility _ (isSource a -> True) n _ | n `elem` [1, 3] -> do
      placeProgress a a 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      returnToStaging a.id
      pure l
    _ -> TheOldBridge <$> runMessage msg a
