module LordOfTheRings.Location.Cards.TheCrossingAtTharbad (theCrossingAtTharbad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheCrossingAtTharbad = TheCrossingAtTharbad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrossingAtTharbad :: LocationCard TheCrossingAtTharbad
theCrossingAtTharbad = location TheCrossingAtTharbad Cards.theCrossingAtTharbad

-- | Immune to player card effects. Cannot leave the staging area. The Crossing at Tharbad
-- gets +2 quest points for each player in the game.
-- ponytail: "cannot leave the staging area" is that it cannot be traveled to; no encounter
-- card of the scenario moves or discards a location otherwise.
instance HasModifiersFor TheCrossingAtTharbad where
  getModifiersFor (TheCrossingAtTharbad a) = do
    players <- length <$> getRemainingPlayers
    modified a a [ImmuneToPlayerCardEffects, CannotBeTraveledTo, QuestPointModifier (2 * players)]

-- | When The Crossing at Tharbad is explored, the players win the game.
instance RunMessage TheCrossingAtTharbad where
  runMessage msg (TheCrossingAtTharbad attrs) = case msg of
    CheckWindows ws | Window When (Explored attrs.id) `elem` ws -> do
      winGame
      TheCrossingAtTharbad <$> runMessage msg attrs
    _ -> TheCrossingAtTharbad <$> runMessage msg attrs
