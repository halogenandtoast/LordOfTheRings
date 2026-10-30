module LordOfTheRings.Event.Cards.ElwingsFlight (elwingsFlight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype ElwingsFlight = ElwingsFlight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Quest Action: Ready a questing character and give that character +1 willpower until the
-- end of the phase. Resolve that effect again for each copy of Elwing's Flight currently in
-- your discard pile (you may choose different targets).
elwingsFlight :: EventCard ElwingsFlight
elwingsFlight =
  eventWith ElwingsFlight Cards.elwingsFlight
    (criteriaL .~ DuringPhase QuestPhase <> CharactersAtLeast 1 CommittedHero CommittedAlly)

-- | Each time the effect resolves is a later step, carrying how many are still to come.
instance RunMessage ElwingsFlight where
  runMessage msg e@(ElwingsFlight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      again <- copiesInDiscard pid card
      doStep again msg
      pure e
    DoStep n msg'@(PlayEvent pid card _) | card.id == toId attrs -> do
      questing <- selectCharacters CommittedHero CommittedAlly
      chooseOneM pid do
        questionLabeled "elwingsFlight.character"
        causedBy attrs
        targets questing \c -> do
          ready c
          phaseModifier attrs c (WillpowerModifier 1)
      when (n > 0) $ doStep (n - 1) msg'
      pure e
    _ -> ElwingsFlight <$> runMessage msg attrs
