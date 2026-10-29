module LordOfTheRings.Event.Cards.PeaceAndThought (peaceAndThought) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype PeaceAndThought = PeaceAndThought EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Refresh Action: Exhaust 2 heroes to draw 5 cards.
peaceAndThought :: EventCard PeaceAndThought
peaceAndThought =
  eventWith PeaceAndThought Cards.peaceAndThought \a ->
    a & criteriaL .~ (DuringPhase RefreshPhase <> CharactersAtLeast 2 (readyHeroes a.owner) (not_ AnyAlly))

readyHeroes :: PlayerId -> HeroMatcher
readyHeroes pid = HeroControlledBy (PlayerWithId pid) <> ReadyHero

instance RunMessage PeaceAndThought where
  runMessage msg e@(PeaceAndThought attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (readyHeroes pid)
      chooseOneM pid do
        questionLabeled "peaceAndThought.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          chooseOneM pid do
            questionLabeled "peaceAndThought.exhaust"
            causedBy attrs
            targets (filter (/= h) heroes) \h' -> do
              exhaust h'
              drawCards attrs pid 5
      pure e
    _ -> PeaceAndThought <$> runMessage msg attrs
