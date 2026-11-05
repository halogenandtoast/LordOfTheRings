module LordOfTheRings.Treachery.Cards.CripplingWeariness (cripplingWeariness) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
newtype CripplingWeariness = CripplingWeariness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cripplingWeariness :: TreacheryCard CripplingWeariness
cripplingWeariness = treachery CripplingWeariness Cards.cripplingWeariness

instance RunMessage CripplingWeariness where
  runMessage msg t@(CripplingWeariness a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> chooseAndExhaust a pid 1
      roundModifier a GameTarget CannotBeReadiedByPlayerCards
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \defender -> roundModifier a defender CannotReady
      pure t
    _ -> CripplingWeariness <$> runMessage msg a
