module LordOfTheRings.Treachery.Cards.FesteringWounds (festeringWounds) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FesteringWounds = FesteringWounds TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

festeringWounds :: TreacheryCard FesteringWounds
festeringWounds = treachery FesteringWounds Cards.festeringWounds

-- | When Revealed: Deal 2 damage to each wounded character.
-- Shadow: Deal 1 damage to each wounded character. (2 damage instead if this attack is
-- undefended.)
instance RunMessage FesteringWounds where
  runMessage msg t@(FesteringWounds attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      wounded <- damagedCharacters AnyHero AnyAlly
      for_ wounded \c -> dealDamage attrs c 2
      pure t
    ResolveShadowEffect cid _ _ defender | cid == attrs.cardId -> do
      wounded <- damagedCharacters AnyHero AnyAlly
      for_ wounded \c -> dealDamage attrs c (ifUndefended defender 2 1)
      pure t
    _ -> FesteringWounds <$> runMessage msg attrs
