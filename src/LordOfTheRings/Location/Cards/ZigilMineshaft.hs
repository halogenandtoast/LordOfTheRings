module LordOfTheRings.Location.Cards.ZigilMineshaft (zigilMineshaft) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ZigilMineshaft = ZigilMineshaft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

zigilMineshaft :: LocationCard ZigilMineshaft
zigilMineshaft = location ZigilMineshaft Cards.zigilMineshaft

-- | Action: Raise each player's threat by 1 to place 1 progress token on Zigil Mineshaft.
instance HasAbilities ZigilMineshaft where
  getAbilities (ZigilMineshaft a) = [anyPlayer $ mkAbility a 1 $ ActionAbility Free]

instance RunMessage ZigilMineshaft where
  runMessage msg l@(ZigilMineshaft a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer (`raiseThreat` 1)
      placeProgress a a 1
      pure l
    _ -> ZigilMineshaft <$> runMessage msg a
