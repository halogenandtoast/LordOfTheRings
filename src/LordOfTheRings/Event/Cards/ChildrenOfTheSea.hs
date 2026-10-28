module LordOfTheRings.Event.Cards.ChildrenOfTheSea (childrenOfTheSea) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype ChildrenOfTheSea = ChildrenOfTheSea EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

elves :: AllyMatcher
elves = AllyMatchAny [AllyWithTrait Silvan, AllyWithTrait Noldor]

-- | Action: Choose a Silvan or Noldor ally you control. That ally gets +2 willpower until
-- the end of the phase. At the end of the phase, shuffle that ally into its owner's deck
-- if it is still in play.
childrenOfTheSea :: EventCard ChildrenOfTheSea
childrenOfTheSea = eventWith ChildrenOfTheSea Cards.childrenOfTheSea \a -> a & criteriaL .~ controlsAlly a.owner elves

instance RunMessage ChildrenOfTheSea where
  runMessage msg e@(ChildrenOfTheSea attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> elves)
      chooseOrRunOneM pid do
        questionLabeled "childrenOfTheSea.chooseAlly"
        causedBy attrs
        targets allies \ally' -> phaseModifiers attrs ally' [WillpowerModifier 2, ShuffleIntoDeckAtEndOfPhase]
      pure e
    _ -> ChildrenOfTheSea <$> runMessage msg attrs
