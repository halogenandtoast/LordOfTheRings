module LordOfTheRings.Treachery.Cards.RansackedSupplies (ransackedSupplies) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RansackedSupplies = RansackedSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ransackedSupplies :: TreacheryCard RansackedSupplies
ransackedSupplies = treachery RansackedSupplies Cards.ransackedSupplies

-- | When Revealed: Each player discards the top card of his deck. Until the end of the
-- phase, add the total cost of each card discarded this way to the Threat in the staging
-- area (printed costs, 0 for a card with none, as Scourge of Mordor's).
-- Shadow: Discard an attachment you control. If this attack destroys a character, discard
-- another attachment you control.
instance RunMessage RansackedSupplies where
  runMessage msg t@(RansackedSupplies attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      let x = sum [printedCostOf c | p <- players, c <- take 1 p.deck]
      for_ players \p -> discardTopOfDeck p.id 1
      phaseModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      discardOne cid pid
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardOne cid pid
      pure t
    _ -> RansackedSupplies <$> runMessage msg attrs
   where
    discardOne cid pid = do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard (CardIdSource cid) pid attachments
