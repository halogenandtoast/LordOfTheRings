module LordOfTheRings.Treachery.Cards.PillagingAndBurning (pillagingAndBurning) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PillagingAndBurning = PillagingAndBurning TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pillagingAndBurning :: TreacheryCard PillagingAndBurning
pillagingAndBurning = treachery PillagingAndBurning Cards.pillagingAndBurning

-- | When Revealed: Each player draws a card. Each player raises his threat by 1 for each
-- card in his hand.
-- Shadow: Defending player discards an attachment he controls (each attachment he
-- controls instead if this attack is undefended).
instance RunMessage PillagingAndBurning where
  runMessage msg t@(PillagingAndBurning attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> drawCards p 1
      -- the hands are counted once the cards are drawn
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      eachPlayer \p -> encounterRaisesThreat p . length . (.hand) =<< getPlayer p
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      ifUndefended defenders (traverse_ discard attachments) (chooseAndDiscard attrs pid attachments)
      pure t
    _ -> PillagingAndBurning <$> runMessage msg attrs
