module LordOfTheRings.Location.Cards.PathlessCountry (pathlessCountry) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PathlessCountry = PathlessCountry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pathlessCountry :: LocationCard PathlessCountry
pathlessCountry = location PathlessCountry Cards.pathlessCountry

-- | Forced: After at least 1 progress token is placed on Pathless Country, remove 1
-- progress token from it.
instance HasAbilities PathlessCountry where
  getAbilities (PathlessCountry a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | Shadow: Deal 1 damage to each ally in play.
instance RunMessage PathlessCountry where
  runMessage msg l@(PathlessCountry a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeTokens a a Progress 1
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      allies <- select AnyAlly
      for_ allies \ally -> dealDamage a ally 1
      pure l
    _ -> PathlessCountry <$> runMessage msg a
