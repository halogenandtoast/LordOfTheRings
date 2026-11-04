module LordOfTheRings.Treachery.Cards.BlazingGrip (blazingGrip) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlazingGrip = BlazingGrip TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Shadow and Flame's Balrog deck, in nightmare mode.
blazingGrip :: TreacheryCard BlazingGrip
blazingGrip = treachery BlazingGrip Cards.blazingGrip

-- | Shadow: Discard all cards attached to the defending character and deal it 1 damage.
-- Cannot be canceled.
-- (Its card def marks the shadow effect as one that cannot be canceled.)
instance RunMessage BlazingGrip where
  runMessage msg t@(BlazingGrip attrs) = case msg of
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> do
        traverse_ discard =<< getAllAttachmentsOn d
        dealDamage attrs d 1
      pure t
    _ -> BlazingGrip <$> runMessage msg attrs
