module LordOfTheRings.Objective.Cards.TheOneRingSaga (theOneRingTheBlackRiders, theOneRingTheRoadDarkens) where

import LordOfTheRings.Helpers.RingBearer (leavesPlay)
import LordOfTheRings.Keyword (Keyword (FirstPlayerGainsControl))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

-- | The Lord of the Rings saga's One Ring, of The Black Riders and The Road Darkens.
-- Setup: The first player claims The One Ring and attaches it to the Ring-bearer: the game
-- does it as it sets the scenario up ('LordOfTheRings.Message.Lifted.setUpFellowship').
newtype TheOneRingSaga = TheOneRingSaga ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOneRingTheBlackRiders :: ObjectiveCard TheOneRingSaga
theOneRingTheBlackRiders = objective TheOneRingSaga Cards.theOneRingTheBlackRiders

theOneRingTheRoadDarkens :: ObjectiveCard TheOneRingSaga
theOneRingTheRoadDarkens = objective TheOneRingSaga Cards.theOneRingTheRoadDarkens

-- | Attached hero does not count against the hero limit: the limit is only the decks' (no
-- more than 3 heroes), and the scenario, not a deck, brings this hero. The first player
-- gains control of the attached hero, following the first player token.
instance HasModifiersFor TheOneRingSaga where
  getModifiersFor (TheOneRingSaga a) = withHost a \host -> modified a host [GainKeyword FirstPlayerGainsControl]

-- | If The One Ring leaves play, the players lose the game.
instance RunMessage TheOneRingSaga where
  runMessage msg (TheOneRingSaga a) = case msg of
    _ | leavesPlay (toTarget a) msg -> do
      gameOver Lost
      TheOneRingSaga <$> runMessage msg a
    _ -> TheOneRingSaga <$> runMessage msg a
