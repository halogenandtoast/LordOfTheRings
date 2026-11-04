module LordOfTheRings.Treachery.Cards.DarkPlaces (darkPlaces) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkPlaces = DarkPlaces TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkPlaces :: TreacheryCard DarkPlaces
darkPlaces = treachery DarkPlaces Cards.darkPlaces

-- | When Revealed: Add the total threat of the active location to the total threat in the
-- staging area until the end of the phase: its threat as it is now. If there is no active
-- location, discard cards from the top of the encounter deck until a location is discarded
-- and add it to the staging area.
-- Shadow: Attacking enemy gets +1 attack.
instance RunMessage DarkPlaces where
  runMessage msg t@(DarkPlaces attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      select IsActiveLocation >>= \case
        lid : _ -> do
          threat <- getLocationThreat lid
          when (threat > 0) $ phaseModifier attrs GameTarget (StagingThreatBonus threat)
        [] -> discardEncounterCardsUntil attrs locationCard
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (find (cardMatches locationCard) cards) addToStagingFromDiscard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure t
    _ -> DarkPlaces <$> runMessage msg attrs
   where
    locationCard = CardWithType LocationType
