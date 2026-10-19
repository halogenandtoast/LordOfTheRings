module LordOfTheRings.Event.Cards.BladeMastery (bladeMastery) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype BladeMastery = BladeMastery EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a character. Until the end of the phase, that character gains +1
-- attack and +1 defense.
bladeMastery :: EventCard BladeMastery
bladeMastery = event BladeMastery Cards.bladeMastery

instance RunMessage BladeMastery where
  runMessage msg e@(BladeMastery attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      characters <- selectCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "chooseCharacter"
        causedBy attrs
        targets characters \c -> phaseModifiers attrs c [AttackModifier 1, DefenseModifier 1]
      pure e
    _ -> BladeMastery <$> runMessage msg attrs
