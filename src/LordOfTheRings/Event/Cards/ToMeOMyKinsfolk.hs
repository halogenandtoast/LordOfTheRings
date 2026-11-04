module LordOfTheRings.Event.Cards.ToMeOMyKinsfolk (toMeOMyKinsfolk) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype ToMeOMyKinsfolk = ToMeOMyKinsfolk EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: If you control at least 1 Dwarf hero, put a Dwarf ally from your discard pile
-- into play under your control. Put that ally on the bottom of your deck at the end of the
-- phase.
toMeOMyKinsfolk :: EventCard ToMeOMyKinsfolk
toMeOMyKinsfolk =
  eventWith ToMeOMyKinsfolk Cards.toMeOMyKinsfolk \a ->
    a
      & criteriaL
      .~ (exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Dwarf) <> CardInDiscard a.owner dwarfAlly)

dwarfAlly :: CardMatcher
dwarfAlly = CardWithType AllyType <> CardWithTrait Dwarf

instance RunMessage ToMeOMyKinsfolk where
  runMessage msg e@(ToMeOMyKinsfolk attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- filter (cardMatches dwarfAlly) . (.discard) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "chooseAllyToPutIntoPlay"
        causedBy attrs
        for_ [c | PlayerCard c <- allies] \c -> targeting c do
          removeFromDiscard pid c.id
          phaseModifier attrs (CardIdTarget c.id) BottomOfDeckAtEndOfPhase
          putAllyIntoPlayUnder pid c
      pure e
    _ -> ToMeOMyKinsfolk <$> runMessage msg attrs
