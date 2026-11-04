module LordOfTheRings.Event.Cards.Reforged (reforged) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype Reforged = Reforged EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose an attachment with a printed cost of X in any player's discard pile and
-- play that attachment for no cost. (The chosen attachment can belong to any sphere of
-- influence.)
--
-- X is paid in Spirit resources once the attachment is chosen, as Stand and Fight's is, so
-- only attachments the player can pay for and play somewhere are offered. Neutral ones are
-- not, as they belong to no sphere: the FAQ rules so for Stand and Fight's same words.
reforged :: EventCard Reforged
reforged = eventWith Reforged Cards.reforged \a -> a & criteriaL .~ AffordableAttachmentInDiscard a.eventOwner Spirit

instance RunMessage Reforged where
  runMessage msg e@(Reforged attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      attachments <- filterM (\(_, pc, _) -> notNull <$> cardHosts pid (toCardCode pc)) =<< affordableDiscardedAttachments pid Spirit
      chooseOneM pid do
        questionLabeled "reforged.choose"
        causedBy attrs
        for_ attachments \(owner, pc, x) -> targeting pc do
          spendResources pid Spirit x
          playFromDiscardAtNoCost owner pid pc
      pure e
    _ -> Reforged <$> runMessage msg attrs
