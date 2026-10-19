module LordOfTheRings.Treachery.Cards.TreacherousFog (treacherousFog) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TreacherousFog = TreacherousFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

treacherousFog :: TreacheryCard TreacherousFog
treacherousFog = treachery TreacherousFog Cards.treacherousFog
