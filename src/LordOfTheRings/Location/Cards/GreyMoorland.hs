module LordOfTheRings.Location.Cards.GreyMoorland (greyMoorland) where

import LordOfTheRings.Card (CardType (TreacheryType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GreyMoorland = GreyMoorland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greyMoorland :: LocationCard GreyMoorland
greyMoorland = location GreyMoorland Cards.greyMoorland

-- | Forced: After Grey Moorland becomes the active location, shuffle the encounter discard
-- pile into the encounter deck and discard cards from it until a treachery is discarded.
-- Resolve the 'When Revealed' effect on that treachery.
instance HasAbilities GreyMoorland where
  getAbilities (GreyMoorland a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The cards are discarded once the shuffled deck is there to discard from.
-- Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage GreyMoorland where
  runMessage msg l@(GreyMoorland a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      discardEncounterCardsUntil a (CardWithType TreacheryType)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ resolveWhenRevealedFromDiscard (find (cardMatches $ CardWithType TreacheryType) cards)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> GreyMoorland <$> runMessage msg a
