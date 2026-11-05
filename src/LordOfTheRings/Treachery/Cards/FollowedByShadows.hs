module LordOfTheRings.Treachery.Cards.FollowedByShadows (followedByShadows) where

import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (failedHideTestThisRound)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FollowedByShadows = FollowedByShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

followedByShadows :: TreacheryCard FollowedByShadows
followedByShadows = treachery FollowedByShadows Cards.followedByShadows

instance RunMessage FollowedByShadows where
  runMessage msg t@(FollowedByShadows a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> hideTest pid a 2
      pure t
    HideTestResult test False | test.hideSource == toSource a -> do
      encounterRaisesThreat test.hidePlayer 5
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      whenM (failedHideTestThisRound pid) $ makeUndefended pid eid
      pure t
    _ -> FollowedByShadows <$> runMessage msg a
