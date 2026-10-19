module LordOfTheRings.Treachery.Cards.EvilStorm (evilStorm) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EvilStorm = EvilStorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilStorm :: TreacheryCard EvilStorm
evilStorm = treachery EvilStorm Cards.evilStorm

-- | When Revealed: Deal 1 damage to each character controlled by each player with a threat
-- of 35 or higher.
instance RunMessage EvilStorm where
  runMessage msg t@(EvilStorm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      order <- getPlayerOrder
      high <- filterM (fmap ((>= 35) . (.threat)) . getPlayer) order
      characters <- concatMapM charactersControlledBy high
      pushAll [DealDamage (toSource attrs) c 1 | c <- characters]
      pure t
    _ -> EvilStorm <$> runMessage msg attrs
