module LordOfTheRings.Event.Cards.DontBeHasty (dontBeHasty) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype DontBeHasty = DontBeHasty EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: When an encounter card is revealed but before resolving any of that card's
-- keywords or "when revealed" effects, choose a character committed to the quest. Ready
-- that character and remove it from the quest.
dontBeHasty :: EventCard DontBeHasty
dontBeHasty =
  eventWith DontBeHasty Cards.dontBeHasty
    $ (windowL ?~ EncounterCardRevealedWindow When)
    . (criteriaL .~ CharactersAtLeast 1 CommittedHero CommittedAlly)

instance RunMessage DontBeHasty where
  runMessage msg e@(DontBeHasty attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy attrs
        targets committed \c -> do
          ready c
          removeFromQuest c
      pure e
    _ -> DontBeHasty <$> runMessage msg attrs
