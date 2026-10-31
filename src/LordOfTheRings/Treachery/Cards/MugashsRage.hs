module LordOfTheRings.Treachery.Cards.MugashsRage (mugashsRage) where

import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (getMugash, mugashCaptor)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MugashsRage = MugashsRage TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mugashsRage :: TreacheryCard MugashsRage
mugashsRage = treachery MugashsRage Cards.mugashsRage

-- | When Revealed: If Mugash is attached to a hero, the player who controls that hero
-- unattaches Mugash and engages it. Then, Mugash makes an immediate attack.
-- Shadow: Attacking enemy cannot take damage until the end of the round.
instance RunMessage MugashsRage where
  runMessage msg t@(MugashsRage attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      captor <- mugashCaptor
      mugash <- getMugash
      for_ ((,) <$> captor <*> mugash) \(pid, m) -> do
        engageEnemy pid m.id
        immediateAttack pid m.id
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure t
    _ -> MugashsRage <$> runMessage msg attrs
