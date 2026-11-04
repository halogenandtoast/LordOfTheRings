module LordOfTheRings.Location.Cards.FieryDepths (fieryDepths) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers (getDurinsBane)

newtype FieryDepths = FieryDepths LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fieryDepths :: LocationCard FieryDepths
fieryDepths = location FieryDepths Cards.fieryDepths

-- | Progress cannot be placed on Fiery Depths while it is in the staging area.
-- With less damage on Durin's Bane than there are players, its Travel cost cannot be paid.
instance HasModifiersFor FieryDepths where
  getModifiersFor (FieryDepths a) = do
    bane <- getDurinsBane
    damage <- maybe (pure 0) (getDamage . toTarget) bane
    players <- length <$> getRemainingPlayers
    modified a a $ [ProgressLimit 0 | a.placement == StagingArea] <> [CannotBeTraveledTo | damage < players]

-- | Travel: Each player must move 1 damage from Durin's Bane to a hero to travel here. The
-- damage is moved, not dealt.
instance RunMessage FieryDepths where
  runMessage msg (FieryDepths a) = case msg of
    TravelTo lid | lid == a.id -> do
      bane <- getDurinsBane
      for_ bane \b -> eachPlayer \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "fieryDepths.moveDamage"
          causedBy a
          targets heroes \h -> moveDamage a b h 1
      FieryDepths <$> runMessage msg a
    _ -> FieryDepths <$> runMessage msg a
