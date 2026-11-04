module LordOfTheRings.Treachery.Cards.Mirage (mirage) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.Types (locationCardId)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Mirage = Mirage TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirage :: TreacheryCard Mirage
mirage = treachery Mirage Cards.mirage

{- | When Revealed: Discard the active location. Shuffle the encounter discard pile into the
encounter deck and discard cards from the top until a location is discarded. Make that
location the active location.

The location found comes into play in the staging area and is then made active, so that
its "after it becomes the active location" effects answer.
-}
instance RunMessage Mirage where
  runMessage msg t@(Mirage attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ discard =<< select IsActiveLocation
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardEncounterCardsUntil attrs (CardWithType LocationType)
      pure t
    DiscardedTopOfEncounterDeck src target cards | isSource attrs src ->
      case find (cardMatches $ CardWithType LocationType) cards of
        Just card -> do
          addToStagingFromDiscard card
          doStep 2 (DiscardedTopOfEncounterDeck src target [card])
          pure t
        Nothing -> pure t
    DoStep 2 (DiscardedTopOfEncounterDeck src _ [card]) | isSource attrs src -> do
      found <- getsGame (find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))
      for_ found \l -> makeActive (toId l)
      pure t
    _ -> Mirage <$> runMessage msg attrs
