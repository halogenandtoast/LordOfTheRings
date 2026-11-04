module LordOfTheRings.Event.Cards.Entmoot (entmoot) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Ent))

newtype Entmoot = Entmoot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control at least 1 Ent character.
entmoot :: EventCard Entmoot
entmoot =
  eventWith Entmoot Cards.entmoot \a ->
    a & criteriaL .~ CharactersAtLeast 1 (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Ent) (AllyControlledBy (PlayerWithId a.owner) <> AllyWithTrait Ent)

-- | Action: Search the top 5 cards of your deck for any number of Ent cards and add them to
-- your hand. Shuffle the other cards back into your deck.
instance RunMessage Entmoot where
  runMessage msg e@(Entmoot attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      searchDeck pid attrs 5 (CardWithTrait Ent) True
      pure e
    _ -> Entmoot <$> runMessage msg attrs
