module LordOfTheRings.Location.Cards.TheHornburg (theHornburg) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TheHornburg = TheHornburg LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theHornburg :: LocationCard TheHornburg
theHornburg = location TheHornburg Cards.theHornburg

-- | While The Hornburg is the active location, reduce the archery total by 1 for each
-- player in the game.
instance HasModifiersFor TheHornburg where
  getModifiersFor (TheHornburg a) = when (a.placement == ActiveLocation) do
    players <- length <$> getRemainingPlayers
    modified a GameTarget [ArcheryBonus (negate players)]

-- | Forced: When The Hornburg is explored, assign X damage among characters in play. X is
-- the number of enemies in play.
instance HasAbilities TheHornburg where
  getAbilities (TheHornburg a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | The first player assigns it, as he makes the final decision for archery damage
-- (The Treason of Saruman rules insert, "Archery X").
instance RunMessage TheHornburg where
  runMessage msg l@(TheHornburg a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      x <- length <$> select AnyEnemy
      characters <- selectCharacters AnyHero AnyAlly
      lead <- getLead
      when (x > 0 && notNull characters) $ dealDamageDividedAmong a lead characters x
      pure l
    _ -> TheHornburg <$> runMessage msg a
