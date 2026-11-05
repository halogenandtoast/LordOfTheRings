module LordOfTheRings.Ally.Cards.EntOfFangorn (entOfFangorn) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

-- | The objective ally of The Road to Isengard, once a player has taken control of it: its
-- text works only in the staging area, so as an ally it has none.
newtype EntOfFangorn = EntOfFangorn AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

entOfFangorn :: AllyCard EntOfFangorn
entOfFangorn = ally EntOfFangorn Cards.entOfFangorn

instance RunMessage EntOfFangorn where
  runMessage msg (EntOfFangorn a) = EntOfFangorn <$> runMessage msg a
