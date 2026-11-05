module LordOfTheRings.Treachery.Cards.DarkInterrogation (darkInterrogation) where

import LordOfTheRings.Attachment.Types (AttachmentAttrs (attachmentController))
import LordOfTheRings.Card (HasCardDef, cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (Entities (entitiesAllies, entitiesAttachments))
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkInterrogation = DarkInterrogation TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkInterrogation :: TreacheryCard DarkInterrogation
darkInterrogation = treachery DarkInterrogation Cards.darkInterrogation

-- | When Revealed: Each player must choose and discard cards from hand until he has only 2
-- cards in hand. (Each player with 2 or fewer cards in hand must instead reveal 1 card from
-- the encounter deck.)
-- Shadow: Discard each card you control that has at least 1 copy of itself in your discard
-- pile. (If this attack is undefended, each player must resolve this effect.)
instance RunMessage DarkInterrogation where
  runMessage msg t@(DarkInterrogation attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        inHand <- length . (.hand) <$> getPlayer pid
        if inHand <= 2
          then revealEncounterCard
          else chooseAndDiscardFromHand pid attrs (inHand - 2)
      pure t
    ResolveShadowEffect cid _ pid defender | cid == attrs.cardId -> do
      players <- if null defender then getRemainingPlayers else pure [pid]
      for_ players \p -> do
        discarded <- map (.name) . (.discard) <$> getPlayer p
        allies <- select (AllyControlledBy $ PlayerWithId p)
        es <- getsGame gameEntities
        let copied :: HasCardDef a => a -> Bool
            copied c = cdName (toCardDef c) `elem` discarded
        traverse_ (discard attrs)
          $ [toTarget a | a <- toList (entitiesAllies es), toId a `elem` allies, copied a]
          <> [toTarget a | a <- toList (entitiesAttachments es), attachmentController (toAttrs a) == Just p, copied a]
      pure t
    _ -> DarkInterrogation <$> runMessage msg attrs
