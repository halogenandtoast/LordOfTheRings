module LordOfTheRings.Event.Cards.VanishFromSight (vanishFromSight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype VanishFromSight = VanishFromSight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control 2 or fewer heroes.
-- Action: Until the end of the phase, if your threat is higher than 20, treat your threat
-- as if it is 20.
vanishFromSight :: EventCard VanishFromSight
vanishFromSight =
  eventWith VanishFromSight Cards.vanishFromSight \a ->
    a & criteriaL .~ NotCriterion (CharactersAtLeast 3 (HeroControlledBy $ PlayerWithId a.owner) (not_ AnyAlly))

instance RunMessage VanishFromSight where
  runMessage msg e@(VanishFromSight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      phaseModifier attrs (PlayerTarget pid) (ThreatCountsAsAtMost 20)
      pure e
    _ -> VanishFromSight <$> runMessage msg attrs
