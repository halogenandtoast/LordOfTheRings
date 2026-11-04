module LordOfTheRings.Event.Cards.HalflingDetermination (halflingDetermination) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype HalflingDetermination = HalflingDetermination EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

halflingDetermination :: EventCard HalflingDetermination
halflingDetermination =
  eventWith HalflingDetermination Cards.halflingDetermination
    $ criteriaL .~ CharactersAtLeast 1 (HeroWithTrait Hobbit) (AllyWithTrait Hobbit)

-- | Action: Choose a Hobbit character. That character gets +2 [willpower], +2 [attack], and
-- +2 [defense] until the end of the phase.
instance RunMessage HalflingDetermination where
  runMessage msg e@(HalflingDetermination attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      hobbits <- selectCharacters (HeroWithTrait Hobbit) (AllyWithTrait Hobbit)
      chooseOrRunOneM pid do
        questionLabeled "halflingDetermination.choose"
        causedBy attrs
        targets hobbits \c -> phaseModifiers attrs c [WillpowerModifier 2, AttackModifier 2, DefenseModifier 2]
      pure e
    _ -> HalflingDetermination <$> runMessage msg attrs
