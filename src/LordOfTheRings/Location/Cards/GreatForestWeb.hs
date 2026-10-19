module LordOfTheRings.Location.Cards.GreatForestWeb (greatForestWeb) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GreatForestWeb = GreatForestWeb LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatForestWeb :: LocationCard GreatForestWeb
greatForestWeb = location GreatForestWeb Cards.greatForestWeb

-- | Travel: Each player must exhaust 1 hero he controls to travel here.
instance HasModifiersFor GreatForestWeb where
  getModifiersFor (GreatForestWeb a) = do
    order <- getPlayerOrder
    stuck <- anyM (\p -> selectNone $ HeroControlledBy (PlayerWithId p) <> ReadyHero) order
    when stuck $ modified a a [CannotBeTraveledTo]

instance RunMessage GreatForestWeb where
  runMessage msg l@(GreatForestWeb a) = case msg of
    TravelTo lid | lid == a.id -> do
      order <- getPlayerOrder
      pushAll [ChooseAndExhaustHero (toSource a) p | p <- order]
      pure l
    _ -> GreatForestWeb <$> runMessage msg a
