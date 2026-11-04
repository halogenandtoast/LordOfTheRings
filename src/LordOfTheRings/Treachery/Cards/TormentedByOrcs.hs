module LordOfTheRings.Treachery.Cards.TormentedByOrcs (tormentedByOrcs) where

import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (guardingAnObjective)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TormentedByOrcs = TormentedByOrcs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tormentedByOrcs :: TreacheryCard TormentedByOrcs
tormentedByOrcs = treachery TormentedByOrcs Cards.tormentedByOrcs

-- | When Revealed: Each player raises his threat by 1 for each guarded objective he owns.
-- If any player does not own a guarded objective, Tormented by Orcs gains surge. Abaan, an
-- encounter card, is owned by no player.
-- Shadow: Until the end of the phase, attacking enemy gets +1 defense (+2 defense instead
-- if it is guarding an objective).
instance RunMessage TormentedByOrcs where
  runMessage msg t@(TormentedByOrcs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      captives <- guardedCharacters
      players <- getRemainingPlayers
      owned <- for players \p -> do
        let n = length [() | (_, Just o) <- captives, o == p]
        encounterRaisesThreat p n
        pure n
      when (any (== 0) owned) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      guarding <- guardingAnObjective =<< getEnemy eid
      phaseModifier attrs eid (DefenseModifier $ if guarding then 2 else 1)
      pure t
    _ -> TormentedByOrcs <$> runMessage msg attrs
