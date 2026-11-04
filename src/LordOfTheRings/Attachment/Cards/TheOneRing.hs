module LordOfTheRings.Attachment.Cards.TheOneRing (theOneRing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype TheOneRing = TheOneRing AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted. Setup: Attach to a hero you control and search your deck for a Master card.
-- Add it to your hand and shuffle your deck. The Setup is the game's, at its setup (see
-- 'setUpTheOneRing'), as the A Shadow in the East rules have it done right after the
-- starting heroes are placed.
theOneRing :: AttachmentCard TheOneRing
theOneRing =
  attachment TheOneRing Cards.theOneRing
    & fmap (overAttrs \a -> a {attachmentAttachTo = AttachToHero (HeroControlledBy $ PlayerWithId a.owner)})

-- | Immune to non-Master card effects. Your threat elimination level is reduced by 5.
instance HasModifiersFor TheOneRing where
  getModifiersFor (TheOneRing a) = do
    modifySelf a [ImmuneToNonMasterCardEffects]
    withHost a \_ -> modified a (PlayerTarget a.controller) [EliminationThreatModifier (-5)]

-- | If The One Ring leaves play, the players lose the game: discarded with its hero, or as
-- the hero's third restricted attachment, it is the game that takes it, not a card effect.
instance RunMessage TheOneRing where
  runMessage msg r@(TheOneRing a) = case msg of
    Discard (isTarget a -> True) -> lose
    ReturnToHand (isTarget a -> True) -> lose
    ShuffleIntoOwnersDeck (isTarget a -> True) -> lose
    PutOnBottomOfOwnersDeck (isTarget a -> True) -> lose
    RemoveFromGame (isTarget a -> True) -> lose
    _ -> TheOneRing <$> runMessage msg a
   where
    lose = do
      gameOver Lost
      pure r
