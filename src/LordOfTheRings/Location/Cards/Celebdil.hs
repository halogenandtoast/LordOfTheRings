module LordOfTheRings.Location.Cards.Celebdil (celebdil) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype Celebdil = Celebdil LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

celebdil :: LocationCard Celebdil
celebdil = location Celebdil Cards.celebdil

-- | While Celebdil is the active location, remove 2 progress tokens from it at the end of
-- each round.
instance HasAbilities Celebdil where
  getAbilities (Celebdil a) =
    [restricted a 1 (LocationExists $ LocationWithId a.id <> IsActiveLocation) $ forced RoundEndsWindow]

instance RunMessage Celebdil where
  runMessage msg l@(Celebdil a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      when (a.progress > 0) $ removeTokens a a Progress (min 2 a.progress)
      pure l
    _ -> Celebdil <$> runMessage msg a
