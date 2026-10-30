module LordOfTheRings.Location.Cards.CoastOfAnfalas (coastOfAnfalas) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype CoastOfAnfalas = CoastOfAnfalas LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

coastOfAnfalas :: LocationCard CoastOfAnfalas
coastOfAnfalas = location CoastOfAnfalas Cards.coastOfAnfalas

readyShips :: AllyMatcher
readyShips = shipObjective <> ReadyAlly

-- | If you are off-course, Coast of Anfalas gets +2 threat.
-- Travel: Exhaust a Ship objective to travel here.
instance HasModifiersFor CoastOfAnfalas where
  getModifiersFor (CoastOfAnfalas a) = do
    off <- isOffCourse
    able <- selectAny readyShips
    modified a a $ [ThreatModifier 2 | off] <> [CannotBeTraveledTo | not able]

-- | Shadow: If you are off-course, attacking enemy gets +2 attack.
instance RunMessage CoastOfAnfalas where
  runMessage msg l@(CoastOfAnfalas a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      ships <- select readyShips
      chooseOrRunOneM lead do
        questionLabeled "coastOfAnfalas.exhaust"
        causedBy a
        targets ships exhaust
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      whenM isOffCourse $ enemyGetsAttack (CardIdSource cid) eid 2
      pure l
    _ -> CoastOfAnfalas <$> runMessage msg a
