module LordOfTheRings.Treachery.Cards.LingeringVenom (lingeringVenom) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LingeringVenom = LingeringVenom TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

lingeringVenom :: TreacheryCard LingeringVenom
lingeringVenom = treachery LingeringVenom Cards.lingeringVenom
