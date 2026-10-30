module LordOfTheRings.Event.Cards.FairAndPerilous (fairAndPerilous) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype FairAndPerilous = FairAndPerilous EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

elves :: (HeroMatcher, AllyMatcher)
elves =
  ( HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan]
  , AllyMatchAny [AllyWithTrait Noldor, AllyWithTrait Silvan]
  )

-- | Action: Choose a Noldor or Silvan character. Until the end of the phase, add that
-- character's willpower to its attack.
fairAndPerilous :: EventCard FairAndPerilous
fairAndPerilous =
  eventWith FairAndPerilous Cards.fairAndPerilous (criteriaL .~ uncurry (CharactersAtLeast 1) elves)

instance RunMessage FairAndPerilous where
  runMessage msg e@(FairAndPerilous attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      characters <- uncurry selectCharacters elves
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy attrs
        targets characters \c -> do
          willpower <- getWillpower c
          phaseModifier attrs c (AttackModifier willpower)
      pure e
    _ -> FairAndPerilous <$> runMessage msg attrs
