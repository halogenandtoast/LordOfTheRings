module LordOfTheRings.Location.Cards.RuinedTower (ruinedTower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea, StagingArea))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype RuinedTower = RuinedTower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinedTower :: LocationCard RuinedTower
ruinedTower = location RuinedTower Cards.ruinedTower

-- | Travel: If there is 1 active location, exhaust a character to travel here. (There are
-- now 2 active locations.)
instance HasModifiersFor RuinedTower where
  getModifiersFor (RuinedTower a) = when (a.placement == StagingArea) do
    active <- length <$> select IsActiveLocation
    able <- selectCharacters ReadyHero ReadyAlly
    modified a a $ TravelableWithActiveLocation : [CannotBeTraveledTo | active == 1, null able]

-- | Forced: After the player who controls Ruined Tower loses control of a location, return
-- Ruined Tower to the staging area.
-- ponytail: read off the location moving out of its play area; no window opens for it
instance RunMessage RuinedTower where
  runMessage msg (RuinedTower a) = case msg of
    -- ponytail: the first player chooses the character, whoever controls it
    TravelTo lid | lid == a.id -> do
      active <- select IsActiveLocation
      unless (null active) do
        lead <- getLead
        able <- selectCharacters ReadyHero ReadyAlly
        chooseOrRunOneM lead do
          questionLabeled "ruinedTower.exhaust"
          causedBy a
          targets able exhaust
      RuinedTower <$> runMessage msg a
    PlaceLocation lid placement | lid /= a.id, controller@(InPlayArea _) <- a.placement -> do
      from <- (.placement) <$> getLocation lid
      when (from == controller && placement /= controller) $ returnToStaging a.id
      RuinedTower <$> runMessage msg a
    _ -> RuinedTower <$> runMessage msg a
