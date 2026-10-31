module LordOfTheRings.Treachery.Cards.RuinAndWroth (ruinAndWroth) where

import LordOfTheRings.Trait (Trait (Battleground))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RuinAndWroth = RuinAndWroth TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinAndWroth :: TreacheryCard RuinAndWroth
ruinAndWroth = treachery RuinAndWroth Cards.ruinAndWroth

-- | Doomed 2.
-- When Revealed: Deal 2 shadow cards to each enemy in play. Deal 2 damage to each
-- Battleground location in play.
-- Shadow: Discard an attachment you control.
instance RunMessage RuinAndWroth where
  runMessage msg t@(RuinAndWroth attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select AnyEnemy
      for_ enemies \e -> replicateM_ 2 (dealShadowCard e)
      battlegrounds <- select (LocationWithTrait Battleground)
      for_ battlegrounds \l -> dealDamage attrs l 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure t
    _ -> RuinAndWroth <$> runMessage msg attrs
