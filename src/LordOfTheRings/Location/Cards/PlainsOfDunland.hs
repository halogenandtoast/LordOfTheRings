module LordOfTheRings.Location.Cards.PlainsOfDunland (plainsOfDunland) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Dunland))

newtype PlainsOfDunland = PlainsOfDunland LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

plainsOfDunland :: LocationCard PlainsOfDunland
plainsOfDunland = location PlainsOfDunland Cards.plainsOfDunland

-- | While Plains of Dunland is in the staging area, each Dunland enemy gets -20 engagement
-- cost and cannot be optionally engaged.
instance HasModifiersFor PlainsOfDunland where
  getModifiersFor (PlainsOfDunland a) = when (a.placement == StagingArea) do
    enemies <- select (EnemyWithTrait Dunland)
    for_ enemies \e -> modified a e [EngagementCostModifier (-20), CannotBeOptionallyEngagedBy AnyPlayer]

-- | Travel: The first player engages the highest attack enemy in the staging area to
-- travel here.
-- ponytail: with no enemy in the staging area the players may still travel here
instance RunMessage PlainsOfDunland where
  runMessage msg l@(PlainsOfDunland a) = case msg of
    TravelTo lid | lid == a.id -> do
      enemies <- select EnemyInStagingArea
      attacks <- traverse (\e -> (e,) <$> getEnemyAttack e) enemies
      let highest = maximumMay (map snd attacks)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "plainsOfDunland.engage"
        targets [e | (e, n) <- attacks, Just n == highest] (engageEnemy lead)
      pure l
    _ -> PlainsOfDunland <$> runMessage msg a
