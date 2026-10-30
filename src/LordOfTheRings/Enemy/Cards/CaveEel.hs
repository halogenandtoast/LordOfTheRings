module LordOfTheRings.Enemy.Cards.CaveEel (caveEel) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)

newtype CaveEel = CaveEel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

caveEel :: EnemyCard CaveEel
caveEel = enemy CaveEel Cards.caveEel

-- | Cave Eel cannot be engaged unless the active location is Underwater.
instance HasModifiersFor CaveEel where
  getModifiersFor (CaveEel a) = unlessM underwaterActive do
    players <- getRemainingPlayers
    modifySelf a (map CannotEngagePlayer players)

-- | Forced: At the beginning of the encounter phase, return Cave Eel to the staging area
-- unless the active location is Underwater.
instance HasAbilities CaveEel where
  getAbilities (CaveEel a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase | EngagedWith _ <- [a.placement]]

instance RunMessage CaveEel where
  runMessage msg e@(CaveEel attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      unlessM underwaterActive $ placeEnemy attrs.id StagingArea
      pure e
    _ -> CaveEel <$> runMessage msg attrs
