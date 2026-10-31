module LordOfTheRings.Location.Cards.ShroudedWilderness (shroudedWilderness) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (flipToNight, isNight)

newtype ShroudedWilderness = ShroudedWilderness LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shroudedWilderness :: LocationCard ShroudedWilderness
shroudedWilderness = location ShroudedWilderness Cards.shroudedWilderness

-- | While it is Night, progress cannot be placed on Shrouded Wilderness.
instance HasModifiersFor ShroudedWilderness where
  getModifiersFor (ShroudedWilderness a) = whenM isNight $ modified a a [ProgressLimit 0]

-- | Travel: Flip the Time objective from Daybreak to Nightfall.
-- Shadow: Attacking enemy gets +1 attack. Then, if it is Night, deal attacking enemy another
-- shadow card.
instance RunMessage ShroudedWilderness where
  runMessage msg l@(ShroudedWilderness a) = case msg of
    TravelTo lid | lid == a.id -> do
      flipToNight
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      whenM isNight $ dealShadowCardsDuringAttack eid pid defenders 1
      pure l
    _ -> ShroudedWilderness <$> runMessage msg a
