module LordOfTheRings.Ally.Cards.DolAmrothWarship (dolAmrothWarship) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype DolAmrothWarship = DolAmrothWarship AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dolAmrothWarship :: AllyCard DolAmrothWarship
dolAmrothWarship = ally DolAmrothWarship Cards.dolAmrothWarship

-- | Immune to player card effects. If you are on-course, Dol Amroth Warship gets +2
-- willpower, +2 attack, and +2 defense.
instance HasModifiersFor DolAmrothWarship where
  getModifiersFor (DolAmrothWarship a) = do
    shipModifiers a
    whenM isOnCourse $ modified a a [WillpowerModifier 2, AttackModifier 2, DefenseModifier 2]

-- | Forced: When Dol Amroth Warship is destroyed, set it aside, out of play.
instance RunMessage DolAmrothWarship where
  runMessage msg (DolAmrothWarship a) =
    DolAmrothWarship <$> runShip (setAside $ MkEncounterCard a.allyCardId (toCardCode a)) msg a
