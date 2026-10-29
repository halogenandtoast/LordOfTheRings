module LordOfTheRings.Hero.Cards.BilboBaggins (bilboBaggins) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BilboBaggins = BilboBaggins HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

bilboBaggins :: HeroCard BilboBaggins
bilboBaggins = hero BilboBaggins Cards.bilboBaggins

-- | The first player draws 1 additional card in the resource phase.
--
-- The additional card is part of the same draw, 2 cards at once, so what stops the next
-- draw, as Iron Shackles, stops both.
instance HasModifiersFor BilboBaggins where
  getModifiersFor (BilboBaggins a) = unless a.heroPrisoner do
    lead <- getLead
    modified a (PlayerTarget lead) [AdditionalResourcePhaseDraws 1]

instance RunMessage BilboBaggins where
  runMessage msg (BilboBaggins attrs) = BilboBaggins <$> runMessage msg attrs
