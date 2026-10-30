module LordOfTheRings.Event.Cards.TaleOfTinuviel (taleOfTinuviel) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (cdWillpower)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Trait (Trait (Dunedain, Noldor))

newtype TaleOfTinuviel = TaleOfTinuviel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | The ready Noldor and Dúnedain characters the player controls, to exhaust.
singers :: PlayerId -> (HeroMatcher, AllyMatcher)
singers pid =
  ( HeroControlledBy you <> ReadyHero <> HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Dunedain]
  , AllyControlledBy you <> ReadyAlly <> AllyMatchAny [AllyWithTrait Noldor, AllyWithTrait Dunedain]
  )
 where
  you = PlayerWithId pid

-- | Action: Exhaust a Noldor character to ready a Dúnedain character, or vice-versa. Until
-- the end of the phase, add the exhausted character's printed willpower to the other
-- character's willpower, attack and defense.
taleOfTinuviel :: EventCard TaleOfTinuviel
taleOfTinuviel =
  eventWith TaleOfTinuviel Cards.taleOfTinuviel \a ->
    a & criteriaL .~ AnyCriteria [exists (fst $ singers a.owner), exists (snd $ singers a.owner)]

instance RunMessage TaleOfTinuviel where
  runMessage msg e@(TaleOfTinuviel attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      exhaustable <- uncurry selectCharacters (singers pid)
      chooseOrRunOneM pid do
        questionLabeled "taleOfTinuviel.exhaust"
        causedBy attrs
        targets exhaustable \x -> do
          exhaust x
          def <- characterCardDef x
          noldor <- hasTrait Noldor x def
          dunedain <- hasTrait Dunedain x def
          let partners t = selectCharacters (HeroWithTrait t) (AllyWithTrait t)
          others <- filter (/= x) . concat <$> sequence ([partners Dunedain | noldor] <> [partners Noldor | dunedain])
          let w = fromMaybe 0 (cdWillpower def)
          chooseOrRunOneM pid do
            questionLabeled "taleOfTinuviel.ready"
            causedBy attrs
            targets (ordNub others) \y -> do
              ready y
              phaseModifiers attrs y [WillpowerModifier w, AttackModifier w, DefenseModifier w]
      pure e
    _ -> TaleOfTinuviel <$> runMessage msg attrs
