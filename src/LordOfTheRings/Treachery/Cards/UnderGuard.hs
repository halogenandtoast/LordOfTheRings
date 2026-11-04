module LordOfTheRings.Treachery.Cards.UnderGuard (underGuard) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnderGuard = UnderGuard TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underGuard :: TreacheryCard UnderGuard
underGuard = treachery UnderGuard Cards.underGuard

-- | When Revealed: Resolve the Travel effect on the active location. If there is no active
-- location, discard cards from the encounter deck until a location is discarded. Add that
-- location to the staging area.
-- Shadow: If this attack destroys a character, resolve the Travel effect on the active
-- location.
instance RunMessage UnderGuard where
  runMessage msg t@(UnderGuard attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      select IsActiveLocation >>= \case
        lid : _ -> resolveTravelEffect lid
        [] -> discardEncounterCardsUntil attrs locationCard
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (find (cardMatches locationCard) cards) addToStagingFromDiscard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      traverse_ resolveTravelEffect =<< select IsActiveLocation
      pure t
    _ -> UnderGuard <$> runMessage msg attrs
   where
    locationCard = CardWithType LocationType
