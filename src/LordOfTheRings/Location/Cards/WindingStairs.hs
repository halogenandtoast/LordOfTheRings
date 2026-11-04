module LordOfTheRings.Location.Cards.WindingStairs (windingStairs) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype WindingStairs = WindingStairs LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

windingStairs :: LocationCard WindingStairs
windingStairs = location WindingStairs Cards.windingStairs

-- | While Winding Stairs is the active location, each enemy in the staging area gets +10
-- engagement cost. A travel cost that cannot be paid bars the travel: not while a player
-- has no ready character to exhaust.
instance HasModifiersFor WindingStairs where
  getModifiersFor (WindingStairs a) = do
    when (a.placement == ActiveLocation) do
      enemies <- select EnemyInStagingArea
      for_ enemies \e -> modified a e [EngagementCostModifier 10]
    players <- getRemainingPlayers
    stuck <- flip anyM players \p -> null <$> selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
    when stuck $ modifySelf a [CannotBeTraveledTo]

-- | Travel: Each player exhausts a character he controls to travel here.
instance RunMessage WindingStairs where
  runMessage msg (WindingStairs a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> chooseAndExhaust a p 1
      WindingStairs <$> runMessage msg a
    _ -> WindingStairs <$> runMessage msg a
