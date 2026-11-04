module LordOfTheRings.Location.Cards.DrearyHills (drearyHills) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DrearyHills = DrearyHills LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drearyHills :: LocationCard DrearyHills
drearyHills = location DrearyHills Cards.drearyHills

-- | Forced: After placing 1 or more progress tokens on Dreary Hills, each player must
-- discard 1 card at random from his hand.
-- Response: After Dreary Hills leaves play as an explored location, Bilbo Baggins gains 1
-- resource.
instance HasAbilities DrearyHills where
  getAbilities (DrearyHills a) =
    [ mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)
    , mkAbility a 2 $ response $ ExploredWindow After a.id
    ]

instance RunMessage DrearyHills where
  runMessage msg l@(DrearyHills a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> discardRandomFromHand p 1
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      traverse_ (\b -> placeResources a b 1) =<< getBilbo
      pure l
    _ -> DrearyHills <$> runMessage msg a
