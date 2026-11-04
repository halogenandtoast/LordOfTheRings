module LordOfTheRings.Treachery.Cards.BitterCold (bitterCold) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BitterCold = BitterCold TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bitterCold :: TreacheryCard BitterCold
bitterCold = treachery BitterCold Cards.bitterCold

{- | When Revealed: Each player must choose: either discard each ally he controls, or raise
his threat by 1 for each ally he controls. A player with no ally has nothing to choose.
Shadow: Attacking enemy makes an additional attack after this one.
-}
instance RunMessage BitterCold where
  runMessage msg t@(BitterCold attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        unless (null allies) $ chooseOneM pid do
          questionLabeled "bitterCold.choose"
          causedBy attrs
          labeled "bitterCold.discard" $ traverse_ discard allies
          labeled "bitterCold.raise" $ encounterRaisesThreat pid (length allies)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> BitterCold <$> runMessage msg attrs
