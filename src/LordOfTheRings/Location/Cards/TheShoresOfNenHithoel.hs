module LordOfTheRings.Location.Cards.TheShoresOfNenHithoel (theShoresOfNenHithoel) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheShoresOfNenHithoel = TheShoresOfNenHithoel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theShoresOfNenHithoel :: LocationCard TheShoresOfNenHithoel
theShoresOfNenHithoel = location TheShoresOfNenHithoel Cards.theShoresOfNenHithoel

-- | Travel: The first player must discard 1 event card from his hand to travel to this
-- location.
instance HasModifiersFor TheShoresOfNenHithoel where
  getModifiersFor (TheShoresOfNenHithoel a) = do
    events <- any ((== EventType) . (.cardType)) . (.hand) <$> (getPlayer =<< getLead)
    unless events $ modified a a [CannotBeTraveledTo]

-- | Shadow: After this attack resolves, return attacking enemy to the staging area.
instance RunMessage TheShoresOfNenHithoel where
  runMessage msg l@(TheShoresOfNenHithoel a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      events <- filter ((== EventType) . (.cardType)) . (.hand) <$> getPlayer lead
      chooseOrRunOneM lead do
        questionLabeled "theShoresOfNenHithoel.discard"
        causedBy a
        targets events $ discardFromHand lead
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> TheShoresOfNenHithoel <$> runMessage msg a
