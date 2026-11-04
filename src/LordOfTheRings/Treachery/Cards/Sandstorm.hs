module LordOfTheRings.Treachery.Cards.Sandstorm (sandstorm) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Sandstorm = Sandstorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sandstorm :: TreacheryCard Sandstorm
sandstorm = treachery Sandstorm Cards.sandstorm

{- | When Revealed: Each player either discards his hand, or assigns X damage among
characters he controls. X is the number of cards in his hand. With no cards in hand there
is nothing to do.
Shadow: Either discard the highest cost attachment you control, or assign X damage among
characters you control where X is the number of attachments you control. With no
attachment there is nothing to do; the printed cost of a claimed objective is 0.
-}
instance RunMessage Sandstorm where
  runMessage msg t@(Sandstorm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        x <- length . (.hand) <$> getPlayer p
        when (x > 0) $ chooseOneM p do
          questionLabeled "sandstorm.choose"
          causedBy attrs
          labeled "sandstorm.discardHand" $ discardHand p
          labeledWith "sandstorm.damage" [intVar "n" x] $ dealDamageDivided attrs p x
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      costs <- getsGame \g -> [(a, maybe 0 printedCostOf (lookup aid g.entities.attachments)) | a@(AttachmentTarget aid) <- attachments] <> [(a, 0) | a <- attachments, not (isAttachment a)]
      let highest = foldr (max . snd) 0 costs
          x = length attachments
      when (x > 0) $ chooseOneM pid do
        questionLabeled "sandstorm.choose"
        causedBy attrs
        labeled "sandstorm.discardAttachment" do
          chooseOrRunOneM pid do
            questionLabeled "sandstorm.attachment"
            causedBy attrs
            targets [a | (a, c) <- costs, c == highest] discard
        labeledWith "sandstorm.damage" [intVar "n" x] $ dealDamageDivided attrs pid x
      pure t
    _ -> Sandstorm <$> runMessage msg attrs
   where
    isAttachment = \case
      AttachmentTarget _ -> True
      _ -> False
