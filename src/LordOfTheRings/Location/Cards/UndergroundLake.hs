module LordOfTheRings.Location.Cards.UndergroundLake (undergroundLake) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype UndergroundLake = UndergroundLake LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

undergroundLake :: LocationCard UndergroundLake
undergroundLake = location UndergroundLake Cards.undergroundLake

-- | X is the number of characters controlled by the player who controls the most
-- characters: its threat and its quest points.
instance HasModifiersFor UndergroundLake where
  getModifiersFor (UndergroundLake a) = do
    x <- mostCharactersControlled
    when (x > 0) $ modifySelf a [ThreatModifier x, QuestPointModifier x]

-- | Forced: After an ally enters play, exhaust it.
instance HasAbilities UndergroundLake where
  getAbilities (UndergroundLake a) = [mkAbility a 1 $ forced $ AllyEnteredPlayWindow After Nothing Nothing]

instance RunMessage UndergroundLake where
  runMessage msg l@(UndergroundLake a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [t | Window After (AllyEnteredPlay t _ _) <- ws] exhaust
      pure l
    _ -> UndergroundLake <$> runMessage msg a
