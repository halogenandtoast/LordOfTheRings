module LordOfTheRings.Attachment.Cards.LorienRope (lorienRope) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Gift (spendBoon)
import LordOfTheRings.Attachment.Import.Lifted

newtype LorienRope = LorienRope AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Road Darkens. Setup: Attach to a hero in play: as the game is set
-- up, from its owner's deck (its card def's setup attachment).
lorienRope :: AttachmentCard LorienRope
lorienRope = attachment LorienRope Cards.lorienRope & attachTo (AttachToHero AnyHero)

-- | Action: Add Lórien Rope to the victory display and remove it from the campaign pool,
-- to give each location in the staging area -2 [threat] until the end of the phase.
instance HasAbilities LorienRope where
  getAbilities (LorienRope a) = [mkAbility a 1 $ ActionAbility Free]

instance RunMessage LorienRope where
  runMessage msg s@(LorienRope a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      spendBoon a
      locations <- select LocationInStagingArea
      for_ locations \l -> phaseModifier a l (ThreatModifier (-2))
      pure s
    _ -> LorienRope <$> runMessage msg a
