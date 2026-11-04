module LordOfTheRings.Location.Cards.GuardTower (guardTower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype GuardTower = GuardTower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardTower :: LocationCard GuardTower
guardTower = location GuardTower Cards.guardTower

{- | While Guard Tower is in the staging area, players cannot draw cards or gain resources
from player card effects. Its Travel cost must be paid to travel here: not while a player
has no ready character to exhaust.
-}
instance HasModifiersFor GuardTower where
  getModifiersFor (GuardTower a) = do
    players <- getRemainingPlayers
    when (a.placement == StagingArea) do
      for_ players \p -> modified a (PlayerTarget p) [CannotDrawCardsFromPlayerCardEffects]
      heroes <- select AnyHero
      for_ heroes \h -> modified a h [CannotGainResourcesFromPlayerCardEffects]
    stuck <- flip anyM players \p -> null <$> selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
    when stuck $ modified a a [CannotBeTraveledTo]

-- | Travel: Each player exhausts a character he controls.
instance RunMessage GuardTower where
  runMessage msg l@(GuardTower a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> chooseAndExhaust a p 1
      pure l
    _ -> GuardTower <$> runMessage msg a
