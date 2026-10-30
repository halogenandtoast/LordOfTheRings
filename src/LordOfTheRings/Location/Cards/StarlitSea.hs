module LordOfTheRings.Location.Cards.StarlitSea (starlitSea) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype StarlitSea = StarlitSea LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

starlitSea :: LocationCard StarlitSea
starlitSea = location StarlitSea Cards.starlitSea

-- | While you are off-course, Starlit Sea gains: "Travel: Raise each player's threat by 3."
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if you are off-course).
instance RunMessage StarlitSea where
  runMessage msg l@(StarlitSea a) = case msg of
    TravelTo lid | lid == a.id -> do
      whenM isOffCourse $ eachPlayer \p -> encounterRaisesThreat p 3
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      off <- isOffCourse
      enemyGetsAttack (CardIdSource cid) eid (if off then 2 else 1)
      pure l
    _ -> StarlitSea <$> runMessage msg a
