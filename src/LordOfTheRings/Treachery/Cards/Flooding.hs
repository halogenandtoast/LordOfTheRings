module LordOfTheRings.Treachery.Cards.Flooding (flooding) where

import LordOfTheRings.Trait (Trait (Riverland))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Flooding = Flooding TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

flooding :: TreacheryCard Flooding
flooding = treachery Flooding Cards.flooding

-- | When Revealed: Remove all progress tokens from all Riverland locations.
-- Shadow: Resolve the "when revealed" effect of this card.
instance RunMessage Flooding where
  runMessage msg t@(Flooding attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      flood
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      flood
      pure t
    _ -> Flooding <$> runMessage msg attrs
   where
    flood = do
      riverlands <- traverse getLocation =<< select (LocationWithTrait Riverland)
      for_ riverlands \l -> when (l.progress > 0) $ removeTokens attrs l Progress l.progress
