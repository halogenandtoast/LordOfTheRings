module LordOfTheRings.Location.Cards.TwistedTunnel (twistedTunnel) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TwistedTunnel = TwistedTunnel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

twistedTunnel :: LocationCard TwistedTunnel
twistedTunnel = location TwistedTunnel Cards.twistedTunnel

-- | When Revealed: Discard cards from the top of the encounter deck until an enemy is
-- discarded. Add that enemy to the staging area.
-- Shadow: Discard the top card of the encounter deck. If that card is an enemy, add it to
-- the staging area. (A shadow card is not in play to read discards back, so the top card is
-- read as it is discarded.)
instance RunMessage TwistedTunnel where
  runMessage msg l@(TwistedTunnel a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      discardEncounterCardsUntil a enemyCard
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find (cardMatches enemyCard) cards) addToStagingFromDiscard
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      getTopOfEncounterDeck >>= traverse_ \top -> do
        discardFromEncounterDeck top
        when (cardMatches enemyCard top) $ addToStagingFromDiscard top
      pure l
    _ -> TwistedTunnel <$> runMessage msg a
   where
    enemyCard = CardWithType EnemyType
