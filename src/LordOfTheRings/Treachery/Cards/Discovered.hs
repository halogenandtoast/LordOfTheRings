module LordOfTheRings.Treachery.Cards.Discovered (discovered) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (placeUnderOrcsSearch, scourCardsInPlay, triggerEachScour)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Discovered = Discovered TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

discovered :: TreacheryCard Discovered
discovered = treachery Discovered Cards.discovered

-- | When Revealed: Either trigger each Scour effect currently in play, or each player places a
-- random card from his hand facedown underneath The Orcs' Search. The first player chooses
-- one that does something: a Scour in play, or a card in some player's hand.
-- Shadow: If this attack destroys a character, place that character facedown underneath The
-- Orcs' Search. Once the attack is over the shadow takes each character it destroyed from
-- its owner's discard pile.
instance RunMessage Discovered where
  runMessage msg t@(Discovered attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      scouring <- notNull <$> scourCardsInPlay
      hands <- any (notNull . (.hand)) <$> (traverse getPlayer =<< getRemainingPlayers)
      chooseOrRunOneM lead do
        questionLabeled "discovered.choose"
        causedBy attrs
        when scouring $ labeled "discovered.scour" triggerEachScour
        when hands $ labeled "discovered.hand" $ eachPlayer \pid -> do
          hand <- (.hand) <$> getPlayer pid
          traverse_ placeUnderOrcsSearch . headMay =<< shuffleM hand
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      destroyed <- map (.card.id) <$> attackDestroyed eid
      discarded <- getsGame (concatMap (.discard) . toList . (.players))
      traverse_ placeUnderOrcsSearch $ filter ((`elem` destroyed) . (.id)) discarded
      pure t
    _ -> Discovered <$> runMessage msg attrs
