module LordOfTheRings.Location.Cards.SiegeTower (siegeTower) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (discardUntilEnemyToStaging)

newtype SiegeTower = SiegeTower LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

siegeTower :: LocationCard SiegeTower
siegeTower = location SiegeTower Cards.siegeTower

-- | While Siege Tower is in the staging area, it gains archery 3.
instance HasModifiersFor SiegeTower where
  getModifiersFor (SiegeTower a) = when (a.placement == StagingArea) $ modifySelf a [GainKeyword (Archery 3)]

-- | Forced: After Siege Tower becomes the active location, shuffle the encounter discard
-- pile into the encounter deck and discard cards from the top until an enemy is discarded.
-- Add the discarded enemy to the staging area.
instance HasAbilities SiegeTower where
  getAbilities (SiegeTower a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The deck is read for the enemy once it has been shuffled.
instance RunMessage SiegeTower where
  runMessage msg l@(SiegeTower a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardUntilEnemyToStaging a
      pure l
    _ -> SiegeTower <$> runMessage msg a
