module LordOfTheRings.Location.Cards.WesternSlopes (westernSlopes) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (enemyProgress, getSmaugTheGolden)

newtype WesternSlopes = WesternSlopes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Its burgle effect is read by burgle attempts only.
westernSlopes :: LocationCard WesternSlopes
westernSlopes = location WesternSlopes Cards.westernSlopes

-- | Response: When Western Slopes leaves play as an explored location (choose one): the
-- first player draws 2 cards, or remove 1 progress token from Smaug the Golden.
instance HasAbilities WesternSlopes where
  getAbilities (WesternSlopes a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

instance RunMessage WesternSlopes where
  runMessage msg l@(WesternSlopes a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lead <- getLead
      smaug <- getSmaugTheGolden
      progress <- maybe (pure 0) enemyProgress smaug
      chooseOrRunOneM pid do
        questionLabeled "westernSlopes.choose"
        causedBy a
        labeled "westernSlopes.draw" $ drawCards a lead 2
        for_ smaug \sg -> when (progress > 0) $ labeled "westernSlopes.removeProgress" $ removeTokens a sg Progress 1
      pure l
    _ -> WesternSlopes <$> runMessage msg a
