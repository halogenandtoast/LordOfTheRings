module LordOfTheRings.Event.Cards.WordOfCommand (wordOfCommand) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Istari))

newtype WordOfCommand = WordOfCommand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust an Istari character to search your deck for 1 card and add it to your
-- hand. Shuffle your deck.
wordOfCommand :: EventCard WordOfCommand
wordOfCommand =
  eventWith WordOfCommand Cards.wordOfCommand \a ->
    a & criteriaL .~ AnyCriteria [exists (istariHeroes a.owner), exists (istariAllies a.owner)]

istariHeroes :: PlayerId -> HeroMatcher
istariHeroes pid = HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Istari <> ReadyHero

istariAllies :: PlayerId -> AllyMatcher
istariAllies pid = AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Istari <> ReadyAlly

instance RunMessage WordOfCommand where
  runMessage msg e@(WordOfCommand attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      istari <- selectCharacters (istariHeroes pid) (istariAllies pid)
      deck <- (.deck) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "wordOfCommand.exhaust"
        causedBy attrs
        targets istari \c -> do
          exhaust c
          searchDeck pid attrs (length deck) AnyCard False
      pure e
    _ -> WordOfCommand <$> runMessage msg attrs
