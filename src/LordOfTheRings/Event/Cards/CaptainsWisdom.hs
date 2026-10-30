module LordOfTheRings.Event.Cards.CaptainsWisdom (captainsWisdom) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Trait (Trait (Noble))

newtype CaptainsWisdom = CaptainsWisdom EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

nobles :: PlayerId -> HeroMatcher
nobles pid = HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Noble <> ReadyHero

-- | Resource Action: Exhaust a Noble hero you control to add 2 resources to that hero's
-- resource pool.
captainsWisdom :: EventCard CaptainsWisdom
captainsWisdom =
  eventWith CaptainsWisdom Cards.captainsWisdom \a ->
    a & criteriaL .~ DuringPhase ResourcePhase <> exists (nobles a.owner)

instance RunMessage CaptainsWisdom where
  runMessage msg e@(CaptainsWisdom attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (nobles pid)
      chooseOrRunOneM pid do
        questionLabeled "captainsWisdom.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          placeResources attrs h 2
      pure e
    _ -> CaptainsWisdom <$> runMessage msg attrs
