module LordOfTheRings.Event.Cards.ToTheEyrie (toTheEyrie) where

import LordOfTheRings.Card (Card (PlayerCard), PlayerCard (..))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Eagle))

newtype ToTheEyrie = ToTheEyrie EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After an ally is destroyed, exhaust 1 Eagle character to move that ally from
-- the discard pile to its owner's hand.
toTheEyrie :: EventCard ToTheEyrie
toTheEyrie =
  eventWith ToTheEyrie Cards.toTheEyrie \a ->
    a
      & windowL ?~ AllyDestroyedWindow After
      & criteriaL .~ AnyCriteria [exists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero <> HeroWithTrait Eagle), exists (AllyControlledBy (PlayerWithId a.owner) <> ReadyAlly <> AllyWithTrait Eagle)]

instance RunMessage ToTheEyrie where
  runMessage msg e@(ToTheEyrie attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      eagles <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> HeroWithTrait Eagle) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> AllyWithTrait Eagle)
      chooseOrRunOneM pid do
        questionLabeled "toTheEyrie.exhaust"
        causedBy attrs
        targets eagles exhaust
      for_ [c | Window _ (CharacterDestroyed (AllyTarget _) (PlayerCard c)) <- windows] \c ->
        for_ c.owner \owner -> returnFromDiscardToHand owner (pcId c)
      pure e
    _ -> ToTheEyrie <$> runMessage msg attrs
