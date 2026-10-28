module LordOfTheRings.Location.Cards.TheDarkWoods (theDarkWoods) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (discardHiddenCard)

newtype TheDarkWoods = TheDarkWoods LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDarkWoods :: LocationCard TheDarkWoods
theDarkWoods = location TheDarkWoods Cards.theDarkWoods

-- | Archery X. X is the number of players in the game.
instance HasModifiersFor TheDarkWoods where
  getModifiersFor (TheDarkWoods a) = do
    players <- length <$> getRemainingPlayers
    modified a a [GainKeyword (Archery players)]

-- | Forced: After The Dark Woods leaves play as an explored location, the first player
-- discards 1 hidden card.
instance HasAbilities TheDarkWoods where
  getAbilities (TheDarkWoods a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage TheDarkWoods where
  runMessage msg l@(TheDarkWoods a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardHiddenCard =<< getLead
      pure l
    _ -> TheDarkWoods <$> runMessage msg a
