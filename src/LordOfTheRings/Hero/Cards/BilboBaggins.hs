module LordOfTheRings.Hero.Cards.BilboBaggins (bilboBaggins) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))

newtype BilboBaggins = BilboBaggins HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

bilboBaggins :: HeroCard BilboBaggins
bilboBaggins = hero BilboBaggins Cards.bilboBaggins

-- | The first player draws 1 additional card in the resource phase.
instance RunMessage BilboBaggins where
  runMessage msg (BilboBaggins attrs) = case msg of
    BeginPhase ResourcePhase | not attrs.heroPrisoner -> do
      lead <- getLead
      drawCards lead 1
      BilboBaggins <$> runMessage msg attrs
    _ -> BilboBaggins <$> runMessage msg attrs
