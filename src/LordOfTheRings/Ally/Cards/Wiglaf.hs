module LordOfTheRings.Ally.Cards.Wiglaf (wiglaf) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Card (CardType (AttachmentType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Item))

newtype Wiglaf = Wiglaf AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wiglaf :: AllyCard Wiglaf
wiglaf = ally Wiglaf Cards.wiglaf

-- | Response: After you play Wiglaf, play an Item attachment on him from your hand for no
-- cost. Action: Exhaust an attachment on Wiglaf to ready him. (Limit once per phase.)
instance HasAbilities Wiglaf where
  getAbilities (Wiglaf a) =
    [ restricted a 1 (CardInHandMatching a.controller (CardWithType AttachmentType <> CardWithTrait Item))
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    , limited (PlayerLimit PerPhase 1)
        $ restricted a 2 (exists $ AllyWithId a.id <> ExhaustedAlly <> AllyWithReadyAttached Nothing)
        $ ActionAbility Free
    ]

instance RunMessage Wiglaf where
  runMessage msg s@(Wiglaf a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      items <- filter (cardMatches (CardWithType AttachmentType <> CardWithTrait Item)) . (.hand) <$> getPlayer pid
      playable <- flip filterM items \c -> andM [passesUnique c, elem (toTarget a) <$> cardHosts pid (toCardCode c)]
      chooseOneM pid do
        questionLabeled "wiglaf.item"
        causedBy a
        targets playable \c -> playOnFromHandAtNoCost pid c a
      pure s
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      ready' <- getsGame \g -> [toTarget x | x <- toList g.entities.attachments, x.placement == AttachedTo (toTarget a), not (attachmentExhausted $ toAttrs x)]
      chooseOrRunOneM pid do
        questionLabeled "wiglaf.exhaust"
        causedBy a
        targets ready' \x -> do
          exhaust x
          ready a
      pure s
    _ -> Wiglaf <$> runMessage msg a
