module LordOfTheRings.Location.Cards.CollapsedMine (collapsedMine) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CollapsedMine = CollapsedMine LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

collapsedMine :: LocationCard CollapsedMine
collapsedMine = location CollapsedMine Cards.collapsedMine

instance HasModifiersFor CollapsedMine where
  getModifiersFor (CollapsedMine a) = when (a.placement == StagingArea) $
    traverse_ (\q -> modified a q [ProgressPerRoundLimit 4]) =<< getCurrentQuest

instance RunMessage CollapsedMine where
  runMessage msg l@(CollapsedMine a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackModifier a eid (AttackDamageRaisesThreatOf pid)
      pure l
    _ -> CollapsedMine <$> runMessage msg a
