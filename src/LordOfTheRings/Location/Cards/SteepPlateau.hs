module LordOfTheRings.Location.Cards.SteepPlateau (steepPlateau) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Uncharted, under a Lost Island side.
newtype SteepPlateau = SteepPlateau LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

steepPlateau :: LocationCard SteepPlateau
steepPlateau = location SteepPlateau Cards.steepPlateau

-- | Response: After Steep Plateau is explored, look at the facedown side of an Uncharted
-- location in the staging area.
-- ponytail: answered as it is explored, while it is still in play to offer it
instance HasAbilities SteepPlateau where
  getAbilities (SteepPlateau a) =
    [restricted a 1 (exists $ LocationInStagingArea <> unchartedLocation) $ response $ ExploredWindow When a.id]

-- | Shadow: Discard the bottom card of the defending player's deck. If that card's printed
-- cost is 2 or less, attacking enemy gets +2 attack.
instance RunMessage SteepPlateau where
  runMessage msg l@(SteepPlateau a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lookAtAnUnchartedLocation pid a
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      bottom <- discardBottomOfDeck pid
      when (maybe False ((<= 2) . printedCostOf) bottom) $ enemyGetsAttack (CardIdSource cid) eid 2
      pure l
    _ -> SteepPlateau <$> runMessage msg a
