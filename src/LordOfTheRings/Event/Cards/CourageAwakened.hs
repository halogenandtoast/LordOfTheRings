module LordOfTheRings.Event.Cards.CourageAwakened (courageAwakened) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype CourageAwakened = CourageAwakened EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a hero. That hero gets +2 willpower until the end of the phase. Then, if
-- your threat is 20 or less and this is the first time you played a copy of Courage
-- Awakened this round, return this card to your hand instead of discarding it.
courageAwakened :: EventCard CourageAwakened
courageAwakened = event CourageAwakened Cards.courageAwakened

instance RunMessage CourageAwakened where
  runMessage msg e@(CourageAwakened attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "chooseHero"
        causedBy attrs
        targets heroes \h -> phaseModifier attrs h (WillpowerModifier 2)
      returnIfFirstCopyThisRound pid card
      pure e
    _ -> CourageAwakened <$> runMessage msg attrs
