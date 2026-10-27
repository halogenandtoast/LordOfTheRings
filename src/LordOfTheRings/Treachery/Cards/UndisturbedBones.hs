module LordOfTheRings.Treachery.Cards.UndisturbedBones (undisturbedBones) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UndisturbedBones = UndisturbedBones TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

undisturbedBones :: TreacheryCard UndisturbedBones
undisturbedBones = treachery UndisturbedBones Cards.undisturbedBones

-- | When Revealed: Each player must deal X damage to 1 ally he controls. X is the number of
-- allies he controls.
-- Shadow: If the defending character is an ally, discard it from play.
instance RunMessage UndisturbedBones where
  runMessage msg t@(UndisturbedBones attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "undisturbedBones.ally"
          causedBy attrs
          targets allies \a -> dealDamage attrs a (length allies)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \case
        d@(AllyTarget _) -> discard d
        _ -> pure ()
      pure t
    _ -> UndisturbedBones <$> runMessage msg attrs
