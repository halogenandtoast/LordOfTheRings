module LordOfTheRings.Event.Cards.ShortCut (shortCut) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype ShortCut = ShortCut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a location enters play, exhaust a Hobbit character to shuffle that
-- location back into the encounter deck. Then, reveal 1 card from the encounter deck and
-- add it to the staging area.
shortCut :: EventCard ShortCut
shortCut =
  eventWith ShortCut Cards.shortCut \a ->
    a
      & windowL
      ?~ LocationEntersPlayWindow After
      & criteriaL
      .~ AnyCriteria [exists (hobbitHeroes a.owner), exists (hobbitAllies a.owner)]

hobbitHeroes :: PlayerId -> HeroMatcher
hobbitHeroes pid = HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Hobbit <> ReadyHero

hobbitAllies :: PlayerId -> AllyMatcher
hobbitAllies pid = AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Hobbit <> ReadyAlly

instance RunMessage ShortCut where
  runMessage msg e@(ShortCut attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      hobbits <- selectCharacters (hobbitHeroes pid) (hobbitAllies pid)
      for_ (take 1 [lid | Window _ (EntersStagingArea (LocationTarget lid)) <- windows]) \lid ->
        chooseOneM pid do
          questionLabeled "shortCut.exhaust"
          causedBy attrs
          aimedAt [lid]
          targets hobbits \h -> do
            exhaust h
            shuffleIntoEncounterDeck lid
            revealEncounterCard
      pure e
    _ -> ShortCut <$> runMessage msg attrs
