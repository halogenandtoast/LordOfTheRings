module LordOfTheRings.Event.Cards.BehindStrongWalls (behindStrongWalls) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype BehindStrongWalls = BehindStrongWalls EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready a defending Gondor character. That character gets +1 defense until the
-- end of the phase.
behindStrongWalls :: EventCard BehindStrongWalls
behindStrongWalls =
  eventWith BehindStrongWalls Cards.behindStrongWalls (criteriaL .~ CharactersAtLeast 1 gondorHero gondorAlly)

gondorHero :: HeroMatcher
gondorHero = HeroWithTrait Gondor <> DefendingHero

gondorAlly :: AllyMatcher
gondorAlly = AllyWithTrait Gondor <> DefendingAlly

instance RunMessage BehindStrongWalls where
  runMessage msg e@(BehindStrongWalls attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      defenders <- selectCharacters gondorHero gondorAlly
      chooseOrRunOneM pid do
        questionLabeled "behindStrongWalls.choose"
        causedBy attrs
        targets defenders \d -> do
          ready d
          phaseModifier attrs d (DefenseModifier 1)
      pure e
    _ -> BehindStrongWalls <$> runMessage msg attrs
