module LordOfTheRings.Location.Cards.PrisonCell (prisonCell) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder, releaseCaptured)

newtype PrisonCell = PrisonCell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1, which the scenario resolves.
prisonCell :: LocationCard PrisonCell
prisonCell = location PrisonCell Cards.prisonCell

-- | Response: After the players travel to Prison Cell, look at the captured card(s)
-- underneath it and choose 1 to put into play at no cost. Place the other cards back
-- underneath Prison Cell.
instance HasAbilities PrisonCell where
  getAbilities (PrisonCell a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

-- | The first player chooses, the others staying where they are.
instance RunMessage PrisonCell where
  runMessage msg l@(PrisonCell a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      held <- capturedUnder a.cardId
      lead <- getLead
      chooseOneM lead do
        questionLabeled "prisonCell.putIntoPlay"
        causedBy a
        showing (map PlayerCard held)
        for_ held \pc -> targeting (CardIdTarget pc.id) $ releaseCaptured a.cardId pc.id
      pure l
    _ -> PrisonCell <$> runMessage msg a
