module LordOfTheRings.Treachery.Cards.VastAndIntricate (vastAndIntricate) where

import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (locateTest, triggerLostEffects)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype VastAndIntricate = VastAndIntricate TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vastAndIntricate :: TreacheryCard VastAndIntricate
vastAndIntricate = treachery VastAndIntricate Cards.vastAndIntricate

-- | When Revealed: The first player makes a locate test. If this test is failed, raise each
-- player's threat by 7, remove all progress tokens from play, and trigger all "Lost:"
-- effects in play.
instance RunMessage VastAndIntricate where
  runMessage msg t@(VastAndIntricate attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locateTest attrs lost (pure ())
      pure t
    _ -> VastAndIntricate <$> runMessage msg attrs
   where
    lost = do
      eachPlayer \p -> encounterRaisesThreat p 7
      removeAllProgress attrs
      triggerLostEffects attrs
