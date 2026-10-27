module LordOfTheRings.Treachery.Cards.DisturbedWaters (disturbedWaters) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

-- | Doomed 5. The keyword is all it does.
newtype DisturbedWaters = DisturbedWaters TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

disturbedWaters :: TreacheryCard DisturbedWaters
disturbedWaters = treachery DisturbedWaters Cards.disturbedWaters
