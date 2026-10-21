module LordOfTheRings.Treachery.Cards.Exhaustion (exhaustion) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Exhaustion = Exhaustion TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

exhaustion :: TreacheryCard Exhaustion
exhaustion = treachery Exhaustion Cards.exhaustion

-- | When Revealed: Deal 2 damage to each exhausted character.
-- Shadow: Deal 1 damage to each exhausted character.
instance RunMessage Exhaustion where
  runMessage msg t@(Exhaustion attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c 2
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c 1
      pure t
    _ -> Exhaustion <$> runMessage msg attrs
