module LordOfTheRings.Event.Cards.DescendantsOfKings (descendantsOfKings) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain))

newtype DescendantsOfKings = DescendantsOfKings EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready up to X Dúnedain characters you control. X is the number of enemies
-- engaged with you.
descendantsOfKings :: EventCard DescendantsOfKings
descendantsOfKings = event DescendantsOfKings Cards.descendantsOfKings

instance RunMessage DescendantsOfKings where
  runMessage msg e@(DescendantsOfKings attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let you = PlayerWithId pid
      x <- length <$> select (EnemyEngagedWith you)
      exhausted <-
        selectCharacters
          (HeroControlledBy you <> HeroWithTrait Dunedain <> ExhaustedHero)
          (AllyControlledBy you <> AllyWithTrait Dunedain <> ExhaustedAlly)
      let readyUpTo :: ReverseQueue n => Int -> [Target] -> n ()
          readyUpTo n cs = when (n > 0 && notNull cs) $ chooseOneM pid do
            questionLabeledWith "descendantsOfKings.ready" [intVar "count" n]
            causedBy attrs
            targets cs \c -> do
              ready c
              readyUpTo (n - 1) (filter (/= c) cs)
            labeled "done" (pure ())
      readyUpTo x exhausted
      pure e
    _ -> DescendantsOfKings <$> runMessage msg attrs
