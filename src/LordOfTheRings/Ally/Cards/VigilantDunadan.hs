module LordOfTheRings.Ally.Cards.VigilantDunadan (vigilantDunadan) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype VigilantDunadan = VigilantDunadan AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

vigilantDunadan :: AllyCard VigilantDunadan
vigilantDunadan = ally VigilantDunadan Cards.vigilantDunadan

-- | While there is a side quest in the victory display, Vigilant Dúnadan does not exhaust
-- to defend.
instance HasModifiersFor VigilantDunadan where
  getModifiersFor (VigilantDunadan a) = do
    n <- getSideQuestsInVictoryDisplay
    when (n > 0) $ modifySelf a [DoesNotExhaustToDefend]

instance RunMessage VigilantDunadan where
  runMessage msg (VigilantDunadan a) = VigilantDunadan <$> runMessage msg a
