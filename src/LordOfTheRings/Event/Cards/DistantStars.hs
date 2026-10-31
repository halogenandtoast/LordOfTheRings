module LordOfTheRings.Event.Cards.DistantStars (distantStars) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard, cdCardType, cdUnique, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Trait (Trait (Ranger, Scout))

newtype DistantStars = DistantStars EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a Ranger or Scout character to discard a non-unique active location.
-- Then, search the encounter deck and discard pile for a non-unique location and make it
-- the active location. Shuffle the encounter deck.
distantStars :: EventCard DistantStars
distantStars =
  eventWith DistantStars Cards.distantStars \a ->
    a
      & criteriaL
      .~ ( exists (IsActiveLocation <> not_ UniqueLocation <> LocationDiscardableByPlayerCards)
             <> uncurry (CharactersAtLeast 1) (scouts a.owner)
         )

scouts :: PlayerId -> (HeroMatcher, AllyMatcher)
scouts pid =
  ( HeroControlledBy you <> ReadyHero <> HeroMatchAny [HeroWithTrait Ranger, HeroWithTrait Scout]
  , AllyControlledBy you <> ReadyAlly <> AllyMatchAny [AllyWithTrait Ranger, AllyWithTrait Scout]
  )
 where
  you = PlayerWithId pid

instance RunMessage DistantStars where
  runMessage msg e@(DistantStars attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      exhaustable <- uncurry selectCharacters (scouts pid)
      active <- select (IsActiveLocation <> not_ UniqueLocation <> LocationDiscardableByPlayerCards)
      scenario <- getsGame (toAttrs . gameScenario)
      let nonUnique :: EncounterCard -> Bool
          nonUnique c = cdCardType (toCardDef c) == LocationType && not (cdUnique $ toCardDef c)
          inDeck = filter nonUnique scenario.encounterDeck
          inDiscard = filter nonUnique scenario.encounterDiscard
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy attrs
        targets exhaustable \c -> do
          exhaust c
          for_ (take 1 active) discard
          chooseOneM pid do
            questionLabeled "distantStars.choose"
            causedBy attrs
            showing (map EncounterCard $ inDeck <> inDiscard)
            targets inDeck \l -> do
              removeFromEncounterDeck l
              putIntoPlayAsActiveLocation l
            targets inDiscard \l -> do
              removeFromEncounterDiscard l
              putIntoPlayAsActiveLocation l
          shuffleEncounterDeck
      pure e
    _ -> DistantStars <$> runMessage msg attrs
