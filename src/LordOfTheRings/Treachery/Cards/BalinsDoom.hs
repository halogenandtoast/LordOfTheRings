module LordOfTheRings.Treachery.Cards.BalinsDoom (balinsDoom) where

import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (addResourceIfGoblin, addResourceToEachGoblin)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BalinsDoom = BalinsDoom TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

balinsDoom :: TreacheryCard BalinsDoom
balinsDoom = treachery BalinsDoom Cards.balinsDoom

-- | When Revealed: Add 1 resource to each Goblin enemy in play.
-- Shadow: If the attacking enemy is a Goblin, add 1 resource to it.
instance RunMessage BalinsDoom where
  runMessage msg t@(BalinsDoom attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addResourceToEachGoblin attrs
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      addResourceIfGoblin attrs eid
      pure t
    _ -> BalinsDoom <$> runMessage msg attrs
