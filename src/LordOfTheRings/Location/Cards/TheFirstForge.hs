module LordOfTheRings.Location.Cards.TheFirstForge (theFirstForge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype TheFirstForge = TheFirstForge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theFirstForge :: LocationCard TheFirstForge
theFirstForge = location TheFirstForge Cards.theFirstForge

-- | Immune to player card effects. The hero with Wormsbane attached cannot ready while The
-- First Forge is the active location.
instance HasModifiersFor TheFirstForge where
  getModifiersFor (TheFirstForge a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    when (a.placement == ActiveLocation) $ wormsbaneHost >>= traverse_ \h -> modified a h [CannotReady]

-- | Forced: After The First Forge becomes the active location, exhaust the hero with
-- Wormsbane attached.
-- Response: After The First Forge is explored, flip Wormsbane to its Reforged side and
-- attach it to any hero.
instance HasAbilities TheFirstForge where
  getAbilities (TheFirstForge a) =
    [ mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)
    , mkAbility a 2 $ response $ ExploredWindow When a.id
    ]

instance RunMessage TheFirstForge where
  runMessage msg l@(TheFirstForge a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ exhaust =<< wormsbaneHost
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      getWormsbane >>= traverse_ \sword -> do
        flipOver (ObjectiveTarget sword)
        lead <- getLead
        heroes <- select AnyHero
        chooseOrRunOneM lead do
          questionLabeled "theFirstForge.attach"
          causedBy a
          targets heroes (attachObjective sword)
      pure l
    _ -> TheFirstForge <$> runMessage msg a
