module LordOfTheRings.Treachery.Cards.Rockslide (rockslide) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Rockslide = Rockslide TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rockslide :: TreacheryCard Rockslide
rockslide = treachery Rockslide Cards.rockslide

-- | When Revealed: Deal 2 damage to each character committed to this quest.
-- Shadow: Remove defending character from combat. This attack is considered undefended.
instance RunMessage Rockslide where
  runMessage msg t@(Rockslide attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      for_ committed \c -> dealDamage attrs c 2
      pure t
    ResolveShadowEffect cid eid pid (Just _) | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure t
    _ -> Rockslide <$> runMessage msg attrs
