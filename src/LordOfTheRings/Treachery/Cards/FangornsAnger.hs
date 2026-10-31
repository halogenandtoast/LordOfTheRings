module LordOfTheRings.Treachery.Cards.FangornsAnger (fangornsAnger) where

import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (mugashHost)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FangornsAnger = FangornsAnger TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fangornsAnger :: TreacheryCard FangornsAnger
fangornsAnger = treachery FangornsAnger Cards.fangornsAnger

-- | When Revealed: If Mugash is attached to a hero, deal 2 damage to that hero. If Mugash is
-- not attached to a hero, Fangorn's Anger gains surge and doomed 2.
-- Shadow: After this attack, if Mugash is attached to a hero, attacking enemy engages the
-- player who controls that hero.
instance RunMessage FangornsAnger where
  runMessage msg t@(FangornsAnger attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      mugashHost >>= \case
        Just hero -> dealDamage attrs hero 2
        Nothing -> do
          raiseThreatFromDoomed 2
          revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      afterAttack eid [DoStep 1 msg]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      captor <- maybe (pure Nothing) characterController =<< mugashHost
      for_ captor \pid -> engageEnemy pid eid
      pure t
    _ -> FangornsAnger <$> runMessage msg attrs
