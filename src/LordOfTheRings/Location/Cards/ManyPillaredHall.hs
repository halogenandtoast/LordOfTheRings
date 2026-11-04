module LordOfTheRings.Location.Cards.ManyPillaredHall (manyPillaredHall) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ManyPillaredHall = ManyPillaredHall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

manyPillaredHall :: LocationCard ManyPillaredHall
manyPillaredHall = location ManyPillaredHall Cards.manyPillaredHall

-- | Many-pillared Hall gets +1 threat for each Many-pillared Hall in play.
instance HasModifiersFor ManyPillaredHall where
  getModifiersFor (ManyPillaredHall a) = do
    halls <- length <$> select (LocationWithCardCode $ toCardCode Cards.manyPillaredHall)
    modifySelf a [ThreatModifier halls]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player's
-- threat is 35 or higher).
instance RunMessage ManyPillaredHall where
  runMessage msg l@(ManyPillaredHall a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      threat <- getThreat pid
      enemyGetsAttack a eid (if threat >= 35 then 2 else 1)
      pure l
    _ -> ManyPillaredHall <$> runMessage msg a
