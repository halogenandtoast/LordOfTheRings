module LordOfTheRings.Location.Cards.FornostSquare (fornostSquare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FornostSquare = FornostSquare LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

fornostSquare :: LocationCard FornostSquare
fornostSquare = location FornostSquare Cards.fornostSquare

-- | Fornost Square gets +1 threat for each resource token on it.
instance HasModifiersFor FornostSquare where
  getModifiersFor (FornostSquare a) = do
    let n = countTokens Resource a.locationTokens
    when (n > 0) $ modified a a [ThreatModifier n]

-- | Forced: After the first player discards any number of cards from the top of his deck,
-- place 1 resource token here.
instance HasAbilities FornostSquare where
  getAbilities (FornostSquare a) = [mkAbility a 1 $ forced $ PlayerDiscardedFromDeckWindow After FirstPlayer]

instance RunMessage FornostSquare where
  runMessage msg l@(FornostSquare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeTokens a a Resource 1
      pure l
    _ -> FornostSquare <$> runMessage msg a
