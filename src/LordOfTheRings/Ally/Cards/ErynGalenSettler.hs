module LordOfTheRings.Ally.Cards.ErynGalenSettler (erynGalenSettler) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype ErynGalenSettler = ErynGalenSettler AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

erynGalenSettler :: AllyCard ErynGalenSettler
erynGalenSettler = ally ErynGalenSettler Cards.erynGalenSettler

-- | The locations in the staging area sharing the active location's title, read as it is
-- explored, while it is still the active location.
sameTitle :: LocationMatcher
sameTitle = LocationInStagingArea <> LocationWithActiveLocationTitle <> LocationDiscardableByPlayerCards

-- | Response: When the active location leaves play as an explored location, discard Eryn
-- Galen Settler to discard a location in the staging area with the same title as the just
-- explored location.
instance HasAbilities ErynGalenSettler where
  getAbilities (ErynGalenSettler a) =
    [restricted a 1 (exists sameTitle) $ response $ ActiveLocationExploredWindow When]

instance RunMessage ErynGalenSettler where
  runMessage msg s@(ErynGalenSettler a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select sameTitle
      discard a
      chooseOrRunOneM pid do
        questionLabeled "chooseLocation"
        causedBy a
        targets locations discard
      pure s
    _ -> ErynGalenSettler <$> runMessage msg a
