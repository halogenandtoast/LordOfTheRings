module LordOfTheRings.Location.Cards.HeartOfFangorn (heartOfFangorn) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype HeartOfFangorn = HeartOfFangorn LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

heartOfFangorn :: LocationCard HeartOfFangorn
heartOfFangorn = location HeartOfFangorn Cards.heartOfFangorn

-- | While Heart of Fangorn is in the staging area, each player cannot refresh more than 5
-- characters during the refresh phase.
instance HasModifiersFor HeartOfFangorn where
  getModifiersFor (HeartOfFangorn a) =
    when (a.placement == StagingArea) do
      players <- getRemainingPlayers
      for_ players \p -> modified a (PlayerTarget p) [RefreshAtMost 5]

-- | Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage HeartOfFangorn where
  runMessage msg l@(HeartOfFangorn a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      additionalAttack pid eid
      pure l
    _ -> HeartOfFangorn <$> runMessage msg a
