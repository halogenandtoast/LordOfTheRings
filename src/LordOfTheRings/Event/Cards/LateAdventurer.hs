module LordOfTheRings.Event.Cards.LateAdventurer (lateAdventurer) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype LateAdventurer = LateAdventurer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Quest Action: Exhaust a character you control that is not committed to the quest to
-- commit that character to the quest.
lateAdventurer :: EventCard LateAdventurer
lateAdventurer =
  eventWith LateAdventurer Cards.lateAdventurer \a ->
    a & criteriaL .~ (DuringPhase QuestPhase <> uncurry (CharactersAtLeast 1) (latecomers a.owner))

latecomers :: PlayerId -> (HeroMatcher, AllyMatcher)
latecomers pid =
  ( HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero
  , AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly
  )

instance RunMessage LateAdventurer where
  runMessage msg e@(LateAdventurer attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      characters <- uncurry selectCharacters (latecomers pid)
      chooseOrRunOneM pid do
        questionLabeled "lateAdventurer.chooseCharacter"
        causedBy attrs
        targets characters \c -> do
          exhaust c
          commitToQuest c
      pure e
    _ -> LateAdventurer <$> runMessage msg attrs
