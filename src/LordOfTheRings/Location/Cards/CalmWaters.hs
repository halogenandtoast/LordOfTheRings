module LordOfTheRings.Location.Cards.CalmWaters (calmWaters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype CalmWaters = CalmWaters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

calmWaters :: LocationCard CalmWaters
calmWaters = location CalmWaters Cards.calmWaters

-- | While Calm Waters is the active location, look at 2 additional cards from the top of
-- the encounter deck during each Sailing test.
instance HasModifiersFor CalmWaters where
  getModifiersFor (CalmWaters a) = when (a.placement == ActiveLocation) $ modified a GameTarget [SailingTestAdditionalCards 2]

-- | Shadow: Attacking enemy gets +1 attack (+3 attack instead if attacking enemy is a Ship
-- enemy).
instance RunMessage CalmWaters where
  runMessage msg l@(CalmWaters a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ship <- selectAny (EnemyWithId eid <> shipEnemy)
      enemyGetsAttack (CardIdSource cid) eid (if ship then 3 else 1)
      pure l
    _ -> CalmWaters <$> runMessage msg a
