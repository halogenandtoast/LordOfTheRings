module LordOfTheRings.Treachery.Cards.SleeplessMalice (sleeplessMalice) where

import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SleeplessMalice = SleeplessMalice TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sleeplessMalice :: TreacheryCard SleeplessMalice
sleeplessMalice = treachery SleeplessMalice Cards.sleeplessMalice

-- | Doomed 2.
-- When Revealed: Each player must discard 3 random cards from his hand or place 1 progress
-- token on To the Tower. A player with an empty hand has nothing to discard, so places it.
-- Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage SleeplessMalice where
  runMessage msg t@(SleeplessMalice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        hand <- (.hand) <$> getPlayer p
        chooseOrRunOneM p do
          questionLabeled "sleeplessMalice.choose"
          causedBy attrs
          unless (null hand) $ labeled "sleeplessMalice.discard" $ discardRandomFromHand p 3
          labeled "sleeplessMalice.progress" $ placeOnTower attrs 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> SleeplessMalice <$> runMessage msg attrs
