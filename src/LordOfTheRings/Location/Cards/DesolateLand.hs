module LordOfTheRings.Location.Cards.DesolateLand (desolateLand) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (randomFromLargestSetAside)

newtype DesolateLand = DesolateLand LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

desolateLand :: LocationCard DesolateLand
desolateLand = location DesolateLand Cards.desolateLand

-- | X is the highest number of characters controlled by a single player.
instance HasModifiersFor DesolateLand where
  getModifiersFor (DesolateLand a) = do
    players <- getRemainingPlayers
    counts <- for players \p -> length <$> selectCharacters (HeroControlledBy $ PlayerWithId p) (AllyControlledBy $ PlayerWithId p)
    modifySelf a [ThreatModifier (foldr max 0 counts)]

instance HasAbilities DesolateLand where
  getAbilities (DesolateLand a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | Forced: After Desolate Land becomes the active location, choose 1 random card from the
-- set aside encounter set with the most cards remaining. Reveal that card as if it was
-- part of the encounter deck.
instance RunMessage DesolateLand where
  runMessage msg l@(DesolateLand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ revealSetAsideCard =<< randomFromLargestSetAside
      pure l
    _ -> DesolateLand <$> runMessage msg a
