module LordOfTheRings.Attachment.Cards.MirrorOfGaladriel (mirrorOfGaladriel) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MirrorOfGaladriel = MirrorOfGaladriel AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Galadriel.
mirrorOfGaladriel :: AttachmentCard MirrorOfGaladriel
mirrorOfGaladriel =
  attachment MirrorOfGaladriel Cards.mirrorOfGaladriel & attachTo (AttachToHero $ HeroWithTitle "Galadriel")

-- | Action: Exhaust Mirror of Galadriel to search the top 10 cards of your deck for a card
-- and add it to your hand. Shuffle the rest back into your deck. Then, discard a random
-- card from your hand.
instance HasAbilities MirrorOfGaladriel where
  getAbilities (MirrorOfGaladriel a) = [mkAbility a 1 $ ActionAbility (ExhaustCost (toTarget a))]

instance RunMessage MirrorOfGaladriel where
  runMessage msg s@(MirrorOfGaladriel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 10 AnyCard False
      discardRandomFromHand pid 1
      pure s
    _ -> MirrorOfGaladriel <$> runMessage msg a
