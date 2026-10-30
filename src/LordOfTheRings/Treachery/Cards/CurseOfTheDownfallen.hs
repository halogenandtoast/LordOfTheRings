module LordOfTheRings.Treachery.Cards.CurseOfTheDownfallen (curseOfTheDownfallen) where

import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CurseOfTheDownfallen = CurseOfTheDownfallen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

curseOfTheDownfallen :: TreacheryCard CurseOfTheDownfallen
curseOfTheDownfallen = treachery CurseOfTheDownfallen Cards.curseOfTheDownfallen

{- | When Revealed: Deal 1 damage to each non-objective ally with printed cost 2 or less.
Place each ally destroyed by this effect on the bottom of its owner's deck.
Shadow: Discard the bottom card of your deck. If that card's printed cost is 2 or less,
attacking enemy makes an additional attack after this one.

Each damaged ally that has gone to its owner's discard pile once the damage is dealt was
destroyed by it, and moves from there.
-}
instance RunMessage CurseOfTheDownfallen where
  runMessage msg t@(CurseOfTheDownfallen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- traverse getAlly =<< select (NotAlly ObjectiveAlly)
      let cheap = filter ((<= 2) . printedCostOf) allies
      for_ cheap \a -> dealDamage attrs a 1
      -- once all the damage is dealt
      traverse_ (allyToBottomOfDeck . toCard) cheap
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      bottom <- discardBottomOfDeck pid
      when (maybe False ((<= 2) . printedCostOf) bottom) $ additionalAttack pid eid
      pure t
    _ -> CurseOfTheDownfallen <$> runMessage msg attrs
