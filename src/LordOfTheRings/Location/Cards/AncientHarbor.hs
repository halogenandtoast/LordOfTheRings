module LordOfTheRings.Location.Cards.AncientHarbor (ancientHarbor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype AncientHarbor = AncientHarbor LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientHarbor :: LocationCard AncientHarbor
ancientHarbor = location AncientHarbor Cards.ancientHarbor

-- | The players cannot travel here.
instance HasModifiersFor AncientHarbor where
  getModifiersFor (AncientHarbor a) = modified a a [CannotBeTraveledTo]

-- | Combat Action: Exhaust a hero to place 1 progress token on Ancient Harbor.
-- ponytail: offered only while it is in the staging area, the one place it can be
-- explored from; under a player's control the progress would explore it again
-- Forced: After the player who controls Ancient Harbor lets an attack go undefended,
-- return Ancient Harbor to the staging area.
instance HasAbilities AncientHarbor where
  getAbilities (AncientHarbor a) =
    [ anyPlayer $ restricted a 1 (DuringPhase CombatPhase) $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]
      <> returnsWhenUndefended a 2

instance RunMessage AncientHarbor where
  runMessage msg l@(AncientHarbor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      returnToStaging a.id
      pure l
    _ -> AncientHarbor <$> runMessage msg a
