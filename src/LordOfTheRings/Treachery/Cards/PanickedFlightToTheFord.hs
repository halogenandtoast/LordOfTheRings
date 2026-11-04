module LordOfTheRings.Treachery.Cards.PanickedFlightToTheFord (panickedFlightToTheFord) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PanickedFlightToTheFord = PanickedFlightToTheFord TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Flight to the Ford burden set. Surge.
panickedFlightToTheFord :: TreacheryCard PanickedFlightToTheFord
panickedFlightToTheFord = treachery PanickedFlightToTheFord Cards.panickedFlightToTheFord

-- | When Revealed: The first player raises his threat by 3 and passes the first player
-- token to the next player, unless an effect keeps it with him.
-- Shadow: Defending player exhausts a character he controls.
instance RunMessage PanickedFlightToTheFord where
  runMessage msg t@(PanickedFlightToTheFord attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      encounterRaisesThreat lead 3
      keeps <- elem CannotLoseFirstPlayerToken <$> getModifiers lead
      unless keeps $ getNextPlayer lead >>= traverse_ setFirstPlayer
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure t
    _ -> PanickedFlightToTheFord <$> runMessage msg attrs
