module LordOfTheRings.Location.Cards.DecrepitCity (decrepitCity) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype DecrepitCity = DecrepitCity LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

decrepitCity :: LocationCard DecrepitCity
decrepitCity = location DecrepitCity Cards.decrepitCity

-- | While Decrepit City is the active location, "when revealed" effects cannot be
-- canceled.
instance HasModifiersFor DecrepitCity where
  getModifiersFor (DecrepitCity a) = when (a.placement == ActiveLocation) $ modified a GameTarget [WhenRevealedCannotBeCanceled]

-- | Shadow: Either discard 3 cards from the top of your deck, or this attack is considered
-- undefended. The defending player chooses.
instance RunMessage DecrepitCity where
  runMessage msg l@(DecrepitCity a) = case msg of
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      chooseOrRunOneM pid do
        questionLabeled "decrepitCity.choose"
        causedBy a
        labeled "decrepitCity.discard" $ discardTopOfDeck pid 3
        unless (null defenders) $ labeled "decrepitCity.undefended" $ doStep 1 msg
      pure l
    -- the attack's damage is still on the game's queue once the choice is made
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> DecrepitCity <$> runMessage msg a
