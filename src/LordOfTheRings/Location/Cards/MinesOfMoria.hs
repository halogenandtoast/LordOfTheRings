module LordOfTheRings.Location.Cards.MinesOfMoria (minesOfMoria) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MinesOfMoria = MinesOfMoria LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

minesOfMoria :: LocationCard MinesOfMoria
minesOfMoria = location MinesOfMoria Cards.minesOfMoria

-- | While Mines of Moria is in the staging area, progress must be placed on Mines of Moria
-- before it can be placed on the current quest: before the active location's too (FAQ, "If
-- Mines of Moria (TRD 52) is in the staging area and there is an active location, where is
-- progress placed first?").
instance HasModifiersFor MinesOfMoria where
  getModifiersFor (MinesOfMoria a) = when (a.placement == StagingArea) $ modifySelf a [TakesProgressBeforeQuest]

-- | Shadow: Defending player discards a non-objective attachment he controls (2
-- non-objective attachments instead if his threat is 35 or higher).
instance RunMessage MinesOfMoria where
  runMessage msg l@(MinesOfMoria a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      threat <- getThreat pid
      discardNonObjectiveAttachment a pid
      when (threat >= 35) $ discardNonObjectiveAttachment a pid
      pure l
    _ -> MinesOfMoria <$> runMessage msg a
