module LordOfTheRings.Treachery.Cards.MassingAtNight (massingAtNight) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MassingAtNight = MassingAtNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

massingAtNight :: TreacheryCard MassingAtNight
massingAtNight = treachery MassingAtNight Cards.massingAtNight
