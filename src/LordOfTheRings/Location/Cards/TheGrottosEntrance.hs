module LordOfTheRings.Location.Cards.TheGrottosEntrance (theGrottosEntrance) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived.Helpers (templeGuardianReturns)

-- | Under a Temple of the Deceived side.
newtype TheGrottosEntrance = TheGrottosEntrance LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theGrottosEntrance :: LocationCard TheGrottosEntrance
theGrottosEntrance = location TheGrottosEntrance Cards.theGrottosEntrance

-- | Immune to player card effects.
instance HasModifiersFor TheGrottosEntrance where
  getModifiersFor (TheGrottosEntrance a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: When The Grotto's Entrance is flipped over while active, put Temple Guardian
-- into play from the victory display, engaged with the player with the highest threat.
instance HasAbilities TheGrottosEntrance where
  getAbilities (TheGrottosEntrance a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

-- | When the 8th progress is placed on The Grotto's Entrance, the players have found the
-- sunken ruins and win the game.
instance RunMessage TheGrottosEntrance where
  runMessage msg l@(TheGrottosEntrance a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      templeGuardianReturns a
      pure l
    PlaceTokens _ (isTarget a -> True) Progress _ -> do
      a' <- runMessage msg a
      when (a'.progress >= 8) winGame
      pure $ TheGrottosEntrance a'
    _ -> TheGrottosEntrance <$> runMessage msg a
