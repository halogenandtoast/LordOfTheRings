module LordOfTheRings.Event.Cards.ElrondsCounsel (elrondsCounsel) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor))

newtype ElrondsCounsel = ElrondsCounsel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: If you control a unique Noldor character, give another character +1
-- willpower until the end of the phase and lower your threat by 3.
elrondsCounsel :: EventCard ElrondsCounsel
elrondsCounsel =
  eventWith ElrondsCounsel Cards.elrondsCounsel \a ->
    a & criteriaL .~ AnyCriteria [exists (noldorHeroes a.owner), controlsAlly a.owner noldorAllies]

-- heroes are all unique
noldorHeroes :: PlayerId -> HeroMatcher
noldorHeroes pid = HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Noldor

noldorAllies :: AllyMatcher
noldorAllies = AllyWithTrait Noldor <> UniqueAlly

instance RunMessage ElrondsCounsel where
  runMessage msg e@(ElrondsCounsel attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      noldor <- selectCharacters (noldorHeroes pid) (AllyControlledBy (PlayerWithId pid) <> noldorAllies)
      others <- filter (\c -> any (/= c) noldor) <$> selectCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "elrondsCounsel.give"
        causedBy attrs
        targets others \c -> phaseModifier attrs c (WillpowerModifier 1)
      lowerThreat pid 3
      pure e
    _ -> ElrondsCounsel <$> runMessage msg attrs
