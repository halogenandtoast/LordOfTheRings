module LordOfTheRings.Treachery.Cards.LeavesOnTree (leavesOnTree) where

import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LeavesOnTree = LeavesOnTree TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

leavesOnTree :: TreacheryCard LeavesOnTree
leavesOnTree = treachery LeavesOnTree Cards.leavesOnTree

-- | Surge.
-- When Revealed: Each player must pay 1 resource for each attachment he controls or
-- discard all attachments he controls.
-- Shadow: Discard an attachment from the defending character.
instance RunMessage LeavesOnTree where
  runMessage msg t@(LeavesOnTree attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        attachments <- getAttachmentsControlledBy p
        let n = length attachments
        able <- (>= n) <$> getResourcesOfSphere p Neutral
        when (n > 0) $ if able
          then chooseOneM p do
            questionLabeled "leavesOnTree.payOrDiscard"
            causedBy attrs
            labeledWith "leavesOnTree.pay" [intVar "n" n] $ spendResources p Neutral n
            labeled "leavesOnTree.discardAll" $ for_ attachments discard
          else for_ attachments discard
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      for_ defenders \d -> chooseAndDiscard attrs pid =<< getAttachmentsOn d
      pure t
    _ -> LeavesOnTree <$> runMessage msg attrs
