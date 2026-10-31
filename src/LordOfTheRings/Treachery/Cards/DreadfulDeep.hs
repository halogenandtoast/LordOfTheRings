module LordOfTheRings.Treachery.Cards.DreadfulDeep (dreadfulDeep) where

import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (chooseLostEffects, locateTest, triggerLostEffects)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DreadfulDeep = DreadfulDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dreadfulDeep :: TreacheryCard DreadfulDeep
dreadfulDeep = treachery DreadfulDeep Cards.dreadfulDeep

-- | When Revealed: Starting with the first player, each player must choose and trigger a
-- different "Lost:" effect in play.
-- Shadow: Make a locate test. If this test is failed, return attacking enemy to the
-- staging area after this attack and trigger all "Lost:" effects in play.
instance RunMessage DreadfulDeep where
  runMessage msg t@(DreadfulDeep attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      chooseLostEffects attrs =<< getRemainingPlayers
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      locateTest attrs (returnToStagingAfterAttack eid >> triggerLostEffects attrs) (pure ())
      pure t
    _ -> DreadfulDeep <$> runMessage msg attrs
