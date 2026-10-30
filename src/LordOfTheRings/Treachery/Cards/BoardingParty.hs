module LordOfTheRings.Treachery.Cards.BoardingParty (boardingParty) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BoardingParty = BoardingParty TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boardingParty :: TreacheryCard BoardingParty
boardingParty = treachery BoardingParty Cards.boardingParty

-- | When Revealed: Each player reveals the top card of the Corsair deck and puts it into
-- play, engaged with him.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if you control an exhausted
-- Ship objective).
instance RunMessage BoardingParty where
  runMessage msg t@(BoardingParty attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer (revealFromCorsairDeck attrs)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      exhausted <- selectAny (shipObjective <> ExhaustedAlly <> AllyControlledBy (PlayerWithId pid))
      enemyGetsAttack (CardIdSource cid) eid (if exhausted then 2 else 1)
      pure t
    _ -> BoardingParty <$> runMessage msg attrs
