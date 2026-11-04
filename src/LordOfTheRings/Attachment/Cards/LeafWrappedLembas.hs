module LordOfTheRings.Attachment.Cards.LeafWrappedLembas (leafWrappedLembas) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Gift (spendBoon)
import LordOfTheRings.Attachment.Import.Lifted

newtype LeafWrappedLembas = LeafWrappedLembas AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Road Darkens. Setup: Attach to a hero in play: as the game is set
-- up, from its owner's deck (its card def's setup attachment).
leafWrappedLembas :: AttachmentCard LeafWrappedLembas
leafWrappedLembas = attachment LeafWrappedLembas Cards.leafWrappedLembas & attachTo (AttachToHero AnyHero)

-- | Action: Add Leaf-wrapped Lembas to the victory display and remove it from the campaign pool,
-- to ready all heroes in play.
instance HasAbilities LeafWrappedLembas where
  getAbilities (LeafWrappedLembas a) = [mkAbility a 1 $ ActionAbility Free]

instance RunMessage LeafWrappedLembas where
  runMessage msg s@(LeafWrappedLembas a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      spendBoon a
      heroes <- select AnyHero
      for_ heroes ready
      pure s
    _ -> LeafWrappedLembas <$> runMessage msg a
