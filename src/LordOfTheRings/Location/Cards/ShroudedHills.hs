module LordOfTheRings.Location.Cards.ShroudedHills (shroudedHills) where

import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ShroudedHills = ShroudedHills LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shroudedHills :: LocationCard ShroudedHills
shroudedHills = location ShroudedHills Cards.shroudedHills

-- | X is the number of quest cards in play. While there is only 1 quest card in play,
-- Shrouded Hills gains surge.
instance HasModifiersFor ShroudedHills where
  getModifiersFor (ShroudedHills a) = do
    x <- questCardsInPlay
    modifySelf a $ [ThreatModifier x | x > 0] <> [GainKeyword Surge | x == 1]

instance RunMessage ShroudedHills where
  runMessage msg (ShroudedHills a) = ShroudedHills <$> runMessage msg a
