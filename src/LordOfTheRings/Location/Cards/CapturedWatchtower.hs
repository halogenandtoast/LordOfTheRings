module LordOfTheRings.Location.Cards.CapturedWatchtower (capturedWatchtower) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)

newtype CapturedWatchtower = CapturedWatchtower LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

capturedWatchtower :: LocationCard CapturedWatchtower
capturedWatchtower = location CapturedWatchtower Cards.capturedWatchtower

instance HasModifiersFor CapturedWatchtower where
  getModifiersFor (CapturedWatchtower a) = do
    crossed <- crossedAnduin
    when crossed $ modifySelf a [ThreatModifier 3]

instance RunMessage CapturedWatchtower where
  runMessage msg l@(CapturedWatchtower attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> CapturedWatchtower <$> runMessage msg attrs
