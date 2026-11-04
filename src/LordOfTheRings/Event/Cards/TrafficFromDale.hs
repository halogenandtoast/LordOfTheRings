module LordOfTheRings.Event.Cards.TrafficFromDale (trafficFromDale) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Dale))

newtype TrafficFromDale = TrafficFromDale EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | You can only play 1 copy of Traffic from Dale each round.
trafficFromDale :: EventCard TrafficFromDale
trafficFromDale =
  eventWith TrafficFromDale Cards.trafficFromDale \a ->
    a
      & criteriaL
      .~ ( DuringPhase PlanningPhase
            <> exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Dale)
            <> TargetLacksModifier (PlayerTarget a.owner) (PlayedCardThisRound (toCardCode Cards.trafficFromDale) EventType)
         )

-- | Planning Action: Choose a Dale hero you control. Add 1 resource to its pool for each
-- character you control with a player attachment.
instance RunMessage TrafficFromDale where
  runMessage msg e@(TrafficFromDale attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let you = PlayerWithId pid
      heroes <- select (HeroControlledBy you <> HeroWithTrait Dale)
      n <- length <$> selectCharacters (HeroControlledBy you <> HeroWithPlayerAttachment) (AllyControlledBy you <> AllyWithPlayerAttachment)
      chooseOrRunOneM pid do
        questionLabeled "trafficFromDale.hero"
        causedBy attrs
        targets heroes \h -> placeResources attrs h n
      pure e
    _ -> TrafficFromDale <$> runMessage msg attrs
