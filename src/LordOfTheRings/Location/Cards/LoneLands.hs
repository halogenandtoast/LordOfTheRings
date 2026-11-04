module LordOfTheRings.Location.Cards.LoneLands (loneLands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype LoneLands = LoneLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

loneLands :: LocationCard LoneLands
loneLands = location LoneLands Cards.loneLands

-- | Forced: After placing 1 or more progress tokens on Lone-Lands, each player removes 1
-- resource from one of his hero's resource pools, if able.
-- Response: After Lone-Lands leaves play as an explored location, Bilbo Baggins gains 1
-- resource.
instance HasAbilities LoneLands where
  getAbilities (LoneLands a) =
    [ mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)
    , mkAbility a 2 $ response $ ExploredWindow After a.id
    ]

instance RunMessage LoneLands where
  runMessage msg l@(LoneLands a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer $ discardResourceFromAHero a
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      traverse_ (\b -> placeResources a b 1) =<< getBilbo
      pure l
    _ -> LoneLands <$> runMessage msg a
