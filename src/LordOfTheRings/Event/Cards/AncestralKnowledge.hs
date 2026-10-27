module LordOfTheRings.Event.Cards.AncestralKnowledge (ancestralKnowledge) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf, Mountain, Underground))

newtype AncestralKnowledge = AncestralKnowledge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a Dwarf character to place 2 progress tokens on the active location.
-- (4 progress tokens instead if it is an Underground or Mountain location.)
ancestralKnowledge :: EventCard AncestralKnowledge
ancestralKnowledge =
  eventWith AncestralKnowledge Cards.ancestralKnowledge
    $ criteriaL
    .~ ( exists IsActiveLocation
          <> AnyCriteria [HeroExists (ReadyHero <> HeroWithTrait Dwarf), AllyExists (ReadyAlly <> AllyWithTrait Dwarf)]
       )

instance RunMessage AncestralKnowledge where
  runMessage msg e@(AncestralKnowledge attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      dwarves <- selectCharacters (ReadyHero <> HeroWithTrait Dwarf) (ReadyAlly <> AllyWithTrait Dwarf)
      active <- select IsActiveLocation
      deep <- or <$> traverse (selectAny . (IsActiveLocation <>) . LocationWithTrait) [Underground, Mountain]
      for_ active \lid -> chooseOneM pid do
        questionLabeled "ancestralKnowledge.exhaust"
        causedBy attrs
        aimedAt [lid]
        targets dwarves \c -> do
          exhaust c
          placeProgress attrs lid (if deep then 4 else 2)
      pure e
    _ -> AncestralKnowledge <$> runMessage msg attrs
