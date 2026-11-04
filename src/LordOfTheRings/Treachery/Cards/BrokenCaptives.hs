module LordOfTheRings.Treachery.Cards.BrokenCaptives (brokenCaptives) where

import LordOfTheRings.Trait (Trait (Warden))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BrokenCaptives = BrokenCaptives TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brokenCaptives :: TreacheryCard BrokenCaptives
brokenCaptives = treachery BrokenCaptives Cards.brokenCaptives

{- | When Revealed: Exhaust each damaged character. If no characters are exhausted by this
effect, Broken Captives gains surge and Doomed 1: a damaged character exhausted already is
not exhausted by it.
Shadow: If attacking enemy is a Warden, it makes an additional attack against you after
this one.
-}
instance RunMessage BrokenCaptives where
  runMessage msg t@(BrokenCaptives attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      damaged <- damagedCharacters ReadyHero ReadyAlly
      for_ damaged exhaust
      when (null damaged) do
        raiseThreatFromDoomed 1
        revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      warden <- selectAny (EnemyWithId eid <> EnemyWithTrait Warden)
      when warden $ additionalAttack pid eid
      pure t
    _ -> BrokenCaptives <$> runMessage msg attrs
