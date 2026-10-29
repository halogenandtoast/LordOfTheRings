module LordOfTheRings.Location.Cards.HithaeglirStream (hithaeglirStream) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype HithaeglirStream = HithaeglirStream LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hithaeglirStream :: LocationCard HithaeglirStream
hithaeglirStream = location HithaeglirStream Cards.hithaeglirStream

-- | While Hithaeglir Stream is the active location, players draw 2 cards during the
-- resource phase instead of 1.
instance HasModifiersFor HithaeglirStream where
  getModifiersFor (HithaeglirStream a) = when (a.placement == ActiveLocation) do
    order <- getPlayerOrder
    for_ order \p -> modified a (PlayerTarget p) [ResourcePhaseDraws 2]

-- | Shadow: Attacking enemy gets +1 attack. Deal it an additional shadow card.
instance RunMessage HithaeglirStream where
  runMessage msg l@(HithaeglirStream a) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      dealShadowCardsDuringAttack eid pid defenders 1
      pure l
    _ -> HithaeglirStream <$> runMessage msg a
