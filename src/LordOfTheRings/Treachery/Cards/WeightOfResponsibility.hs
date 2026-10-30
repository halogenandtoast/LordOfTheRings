module LordOfTheRings.Treachery.Cards.WeightOfResponsibility (weightOfResponsibility) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeightOfResponsibility = WeightOfResponsibility TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weightOfResponsibility :: TreacheryCard WeightOfResponsibility
weightOfResponsibility = treachery WeightOfResponsibility Cards.weightOfResponsibility

-- | When Revealed: Reveal 1 encounter card for each quest card in play.
-- Shadow: Defending player discards 1 attachment he controls for each quest card in play:
-- one at a time, each chosen from those he still controls.
instance RunMessage WeightOfResponsibility where
  runMessage msg t@(WeightOfResponsibility attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealEncounterCards =<< questCardsInPlay
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      n <- questCardsInPlay
      doStep n msg
      pure t
    DoStep n msg'@(ResolveShadowEffect cid _ pid _) | cid == attrs.cardId, n > 0 -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) do
        chooseAndDiscard attrs pid attachments
        doStep (n - 1) msg'
      pure t
    _ -> WeightOfResponsibility <$> runMessage msg attrs
