module LordOfTheRings.Treachery.Cards.IceStorm (iceStorm) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IceStorm = IceStorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

iceStorm :: TreacheryCard IceStorm
iceStorm = treachery IceStorm Cards.iceStorm

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- the number of quest cards in play. Then, exhaust each damaged character.
-- Shadow: Assign X damage among characters you control. X is the number of quest cards in
-- play.
instance RunMessage IceStorm where
  runMessage msg t@(IceStorm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- questCardsInPlay
      eachPlayer \p -> dealDamageDivided attrs p x
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      traverse_ exhaust =<< damagedCharacters AnyHero AnyAlly
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      dealDamageDivided attrs pid =<< questCardsInPlay
      pure t
    _ -> IceStorm <$> runMessage msg attrs
