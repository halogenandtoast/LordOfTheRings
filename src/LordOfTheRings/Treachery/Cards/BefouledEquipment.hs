module LordOfTheRings.Treachery.Cards.BefouledEquipment (befouledEquipment) where

import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (shadowGuardTargets, tantrumTargets)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BefouledEquipment = BefouledEquipment TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

befouledEquipment :: TreacheryCard BefouledEquipment
befouledEquipment = treachery BefouledEquipment Cards.befouledEquipment

-- | When Revealed: Discard all attachments controlled by the player guarding Gollum. Then,
-- remove all cards in that player's discard pile from the game. If no attachments are
-- discarded as a result of this effect, Befouled Equipment gains surge.
-- Shadow: Discard all attachments controlled by the player guarding Gollum.
instance RunMessage BefouledEquipment where
  runMessage msg t@(BefouledEquipment attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- tantrumTargets
      discarded <- for players \p -> do
        attachments <- getAttachmentsControlledBy p
        traverse_ discard attachments
        cards <- (.discard) <$> getPlayer p
        for_ cards \c -> removeFromDiscard p c.id
        pure attachments
      when (all null discarded) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      shadowGuardTargets eid >>= traverse_ (traverse_ discard <=< getAttachmentsControlledBy)
      pure t
    _ -> BefouledEquipment <$> runMessage msg attrs
