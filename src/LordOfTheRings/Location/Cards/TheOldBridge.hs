module LordOfTheRings.Location.Cards.TheOldBridge (theOldBridge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
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
-- ponytail: offered only while it is in the staging area, the one place it can be
-- explored from; and the resources are spent as it resolves, not paid as its cost, which
-- nothing can come between
-- Forced: After the player who controls The Old Bridge lets an attack go undefended,
-- return The Old Bridge to the staging area.
instance HasAbilities TheOldBridge where
  getAbilities (TheOldBridge a) =
    [ anyPlayer
        $ restricted
          a
          1
          ( DuringPhase PlanningPhase
              <> AnyCriteria
                [ ResourcesOfSphere ActivePlayer Neutral 2
                , exists (anotherOsgiliath a) <> ResourcesOfSphere ActivePlayer Neutral 1
                ]
          )
        $ ActionAbility Free
    | a.placement == StagingArea
    ]
      <> returnsWhenUndefended a 2

anotherOsgiliath :: LocationAttrs -> LocationMatcher
anotherOsgiliath a = LocationInStagingArea <> LocationWithTrait Osgiliath <> not_ (LocationWithId a.id)

instance RunMessage TheOldBridge where
  runMessage msg l@(TheOldBridge a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      cheaper <- selectAny (anotherOsgiliath a)
      spendResources pid Neutral (if cheaper then 1 else 2)
      placeProgress a a 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      returnToStaging a.id
      pure l
    _ -> TheOldBridge <$> runMessage msg a
