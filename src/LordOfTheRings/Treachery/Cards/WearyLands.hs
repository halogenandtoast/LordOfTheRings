module LordOfTheRings.Treachery.Cards.WearyLands (wearyLands) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WearyLands = WearyLands TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wearyLands :: TreacheryCard WearyLands
wearyLands = treachery WearyLands Cards.wearyLands

-- | Doomed 1.
-- When Revealed: Each exhausted character gets -1 Willpower until the end of the phase:
-- those exhausted as it resolves.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage WearyLands where
  runMessage msg t@(WearyLands attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> phaseModifier attrs c (WillpowerModifier (-1))
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> WearyLands <$> runMessage msg attrs
