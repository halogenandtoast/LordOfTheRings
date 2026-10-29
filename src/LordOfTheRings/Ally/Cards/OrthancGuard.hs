module LordOfTheRings.Ally.Cards.OrthancGuard (orthancGuard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype OrthancGuard = OrthancGuard AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orthancGuard :: AllyCard OrthancGuard
orthancGuard = ally OrthancGuard Cards.orthancGuard

-- | Response: After you raise your threat from the Doomed keyword, ready Orthanc Guard.
instance HasAbilities OrthancGuard where
  getAbilities (OrthancGuard a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly) $ response $ ThreatRaisedByDoomedWindow After pid
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage OrthancGuard where
  runMessage msg s@(OrthancGuard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> OrthancGuard <$> runMessage msg a
