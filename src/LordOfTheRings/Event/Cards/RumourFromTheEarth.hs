module LordOfTheRings.Event.Cards.RumourFromTheEarth (rumourFromTheEarth) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype RumourFromTheEarth = RumourFromTheEarth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Look at the top card of the encounter deck. Then, you may pay 1 Lore resource to
-- return Rumour from the Earth to your hand.
rumourFromTheEarth :: EventCard RumourFromTheEarth
rumourFromTheEarth = event RumourFromTheEarth Cards.rumourFromTheEarth

instance RunMessage RumourFromTheEarth where
  runMessage msg e@(RumourFromTheEarth attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      lookAtTopOfEncounterDeck pid attrs False
      lore <- getResourcesOfSphere pid Lore
      when (lore > 0) $ chooseOneM pid do
        questionLabeled "rumourFromTheEarth.return"
        causedBy attrs
        labeled "payResource" do
          spendResources pid Lore 1
          returnFromDiscardToHand pid card.id
        labeled "pass" $ pure ()
      pure e
    _ -> RumourFromTheEarth <$> runMessage msg attrs
