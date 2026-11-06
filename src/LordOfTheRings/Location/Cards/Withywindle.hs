module LordOfTheRings.Location.Cards.Withywindle (withywindle) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype Withywindle = Withywindle LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

withywindle :: LocationCard Withywindle
withywindle = location Withywindle Cards.withywindle

instance HasModifiersFor Withywindle where
  getModifiersFor (Withywindle a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    modified a GameTarget $ PlayersCannotWin : [AdditionalStagingReveals 1 | a.placement == ActiveLocation]
