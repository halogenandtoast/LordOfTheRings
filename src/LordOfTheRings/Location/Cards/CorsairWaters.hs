module LordOfTheRings.Location.Cards.CorsairWaters (corsairWaters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype CorsairWaters = CorsairWaters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairWaters :: LocationCard CorsairWaters
corsairWaters = location CorsairWaters Cards.corsairWaters

-- | While Corsair Waters is the active location, no more than 1 ally can enter play each
-- round.
-- Played or put into play, each counts, and once one has none other can.
instance HasModifiersFor CorsairWaters where
  getModifiersFor (CorsairWaters a) = when (a.placement == ActiveLocation) $ modified a GameTarget [OneAllyEntersPlayPerRound]

-- | Shadow: Either shift your heading off-course, or treat this attack as undefended. An
-- undefended attack already is, so it asks nothing.
instance RunMessage CorsairWaters where
  runMessage msg l@(CorsairWaters a) = case msg of
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      unless (null defenders) do
        chooseOneM pid do
          questionLabeled "corsairWaters.choose"
          causedBy a
          labeled "shiftHeadingOffCourse" $ shiftHeadingOffCourse a
          -- the attack's own messages are on the game's queue once the choice is made
          labeled "corsairWaters.undefended" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> CorsairWaters <$> runMessage msg a
