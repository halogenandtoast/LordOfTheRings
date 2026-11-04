module LordOfTheRings.Objective.Cards.TheOneRingSaga (
  theOneRingTheBlackRiders,
  theOneRingTheRoadDarkens,
  theOneRingTheLandOfShadow,
  theOneRingTheMountainOfFire,
) where

import LordOfTheRings.Keyword (Keyword (FirstPlayerGainsControl))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

{- | The One Ring of The Black Riders, The Road Darkens, The Land of Shadow and The Mountain
of Fire. Setup: The first player claims The One Ring and attaches it to the Ring-bearer: the
game does it as the scenario is set up (see 'setUpFellowship'). Attached hero does not
count against the hero limit, which only deckbuilding counts here: the scenario supplies
the hero, apart from the deck.
-}
newtype TheOneRingSaga = TheOneRingSaga ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOneRingTheBlackRiders :: ObjectiveCard TheOneRingSaga
theOneRingTheBlackRiders = objective TheOneRingSaga Cards.theOneRingTheBlackRiders

theOneRingTheRoadDarkens :: ObjectiveCard TheOneRingSaga
theOneRingTheRoadDarkens = objective TheOneRingSaga Cards.theOneRingTheRoadDarkens

theOneRingTheLandOfShadow :: ObjectiveCard TheOneRingSaga
theOneRingTheLandOfShadow = objective TheOneRingSaga Cards.theOneRingTheLandOfShadow

theOneRingTheMountainOfFire :: ObjectiveCard TheOneRingSaga
theOneRingTheMountainOfFire = objective TheOneRingSaga Cards.theOneRingTheMountainOfFire

-- | The first player gains control of attached hero.
instance HasModifiersFor TheOneRingSaga where
  getModifiersFor (TheOneRingSaga a) = withHost a \host -> modified a host [GainKeyword FirstPlayerGainsControl]

-- | If The One Ring leaves play, the players lose the game.
instance RunMessage TheOneRingSaga where
  runMessage msg r@(TheOneRingSaga a) = case msg of
    _ | leavesPlay (toTarget a) msg -> do
      gameOver Lost
      pure r
    _ -> TheOneRingSaga <$> runMessage msg a
