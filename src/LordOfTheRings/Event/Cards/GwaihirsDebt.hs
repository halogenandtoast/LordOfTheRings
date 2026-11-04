module LordOfTheRings.Event.Cards.GwaihirsDebt (gwaihirsDebt) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Eagle, Istari))

newtype GwaihirsDebt = GwaihirsDebt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Istari trait and another unique
-- character with the Eagle trait.
gwaihirsDebt :: EventCard GwaihirsDebt
gwaihirsDebt =
  eventWith GwaihirsDebt Cards.gwaihirsDebt \a ->
    a & criteriaL .~ DistinctCharacters [uniqueWith a.owner Istari, uniqueWith a.owner Eagle]
 where
  uniqueWith pid trait =
    ( HeroControlledBy (PlayerWithId pid) <> HeroWithTrait trait
    , AllyControlledBy (PlayerWithId pid) <> AllyWithTrait trait <> UniqueAlly
    )

-- | Action: Search the top 5 cards of your deck for an Eagle or Istari ally and put it into
-- play. Shuffle your deck. At the end of the round, if that ally is still in play, return
-- it to your hand.
instance RunMessage GwaihirsDebt where
  runMessage msg e@(GwaihirsDebt attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      searchDeckIntoPlay pid attrs 5 (CardWithType AllyType <> CardMatchAny [CardWithTrait Eagle, CardWithTrait Istari]) 1 [ReturnToHandAtEndOfRound]
      pure e
    _ -> GwaihirsDebt <$> runMessage msg attrs
