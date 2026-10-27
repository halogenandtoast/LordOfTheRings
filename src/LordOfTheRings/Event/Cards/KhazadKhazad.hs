module LordOfTheRings.Event.Cards.KhazadKhazad (khazadKhazad) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype KhazadKhazad = KhazadKhazad EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a Dwarf character. Until the end of the phase, that character gets
-- +3 attack.
khazadKhazad :: EventCard KhazadKhazad
khazadKhazad =
  eventWith KhazadKhazad Cards.khazadKhazad (criteriaL .~ AnyCriteria [HeroExists (HeroWithTrait Dwarf), AllyExists (AllyWithTrait Dwarf)])

instance RunMessage KhazadKhazad where
  runMessage msg e@(KhazadKhazad attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      dwarves <- selectCharacters (HeroWithTrait Dwarf) (AllyWithTrait Dwarf)
      chooseOneM pid do
        questionLabeled "khazadKhazad.chooseDwarf"
        causedBy attrs
        targets dwarves \c -> phaseModifier attrs c (AttackModifier 3)
      pure e
    _ -> KhazadKhazad <$> runMessage msg attrs
