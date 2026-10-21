module LordOfTheRings.Event.Cards.DurinsSong (durinsSong) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DurinsSong = DurinsSong EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a Dwarf hero. That hero gets +2 willpower, +2 attack, and +2 defense
-- until the end of the round.
durinsSong :: EventCard DurinsSong
durinsSong = eventWith DurinsSong Cards.durinsSong (criteriaL .~ exists (HeroWithTrait Dwarf))

instance RunMessage DurinsSong where
  runMessage msg e@(DurinsSong attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      dwarves <- select (HeroWithTrait Dwarf)
      chooseOneM pid do
        questionLabeled "durinsSong.chooseHero"
        causedBy attrs
        targets dwarves \h -> roundModifiers attrs h [WillpowerModifier 2, AttackModifier 2, DefenseModifier 2]
      pure e
    _ -> DurinsSong <$> runMessage msg attrs
