module LordOfTheRings.Treachery.Cards.PainAssailedHim (painAssailedHim) where

import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PainAssailedHim = PainAssailedHim TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

painAssailedHim :: TreacheryCard PainAssailedHim
painAssailedHim = treachery PainAssailedHim Cards.painAssailedHim

-- | When Revealed: Reduce the Ring-bearer's life by 1. Then, reduce the Ring-bearer's
-- willpower, attack, and defense to 0 until the end of the round.
-- Shadow: If this attack destroys a character, reduce the Ring-bearer's life by 1.
instance RunMessage PainAssailedHim where
  runMessage msg t@(PainAssailedHim attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      reduceRingBearersLife 1
      bearers <- select ringBearer
      for_ bearers \h -> roundModifiers attrs h [WillpowerReducedToZero, DoesNotCountAttack, DoesNotCountDefense]
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      reduceRingBearersLife 1
      pure t
    _ -> PainAssailedHim <$> runMessage msg attrs
