module LordOfTheRings.Attachment.Cards.ThrorsGoldenCup (throrsGoldenCup) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrorsGoldenCup = ThrorsGoldenCup AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Thrór's Golden Cup of The Lonely Mountain. Attach to a hero.
throrsGoldenCup :: AttachmentCard ThrorsGoldenCup
throrsGoldenCup = attachment ThrorsGoldenCup Cards.throrsGoldenCup

-- | Action: Add Thrór's Golden Cup to the victory display to draw 3 cards.
instance HasAbilities ThrorsGoldenCup where
  getAbilities (ThrorsGoldenCup a) = [mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage ThrorsGoldenCup where
  runMessage msg s@(ThrorsGoldenCup a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      addToVictoryDisplay a
      drawCards a pid 3
      pure s
    _ -> ThrorsGoldenCup <$> runMessage msg a
