module LordOfTheRings.Event.Cards.TightenOurBelts (tightenOurBelts) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype TightenOurBelts = TightenOurBelts EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Only 1 copy of Tighten Our Belts can be played by the players each round.
tightenOurBelts :: EventCard TightenOurBelts
tightenOurBelts =
  eventWith TightenOurBelts Cards.tightenOurBelts
    $ criteriaL
    .~ (DuringPhase RefreshPhase <> NotCriterion (CardPlayedThisRound $ toCardCode Cards.tightenOurBelts))

-- | Refresh Action: Choose a player. Each hero that player controls that did not spend any
-- resources this round gains 1 resource.
instance RunMessage TightenOurBelts where
  runMessage msg e@(TightenOurBelts attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        thrifty <- filterM (fmap (notElem SpentResourcesThisRound) . getModifiers) heroes
        for_ thrifty \h -> placeResources attrs h 1
      pure e
    _ -> TightenOurBelts <$> runMessage msg attrs
