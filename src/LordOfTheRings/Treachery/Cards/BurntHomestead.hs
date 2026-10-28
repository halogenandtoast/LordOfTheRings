module LordOfTheRings.Treachery.Cards.BurntHomestead (burntHomestead) where

import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BurntHomestead = BurntHomestead TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

burntHomestead :: TreacheryCard BurntHomestead
burntHomestead = treachery BurntHomestead Cards.burntHomestead

-- | Surge.
-- When Revealed: Raise each player's threat by the number of damage tokens on Dead
-- Villagers.
-- Shadow: Defending player raises his threat by 1 for each damage token on Dead Villagers.
instance RunMessage BurntHomestead where
  runMessage msg t@(BurntHomestead attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      dead <- deadVillagerCount
      eachPlayer \p -> encounterRaisesThreat p dead
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid =<< deadVillagerCount
      pure t
    _ -> BurntHomestead <$> runMessage msg attrs
