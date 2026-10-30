module LordOfTheRings.Treachery.Cards.CenturiesOfSorrow (centuriesOfSorrow) where

import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (controlsClue)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CenturiesOfSorrow = CenturiesOfSorrow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

centuriesOfSorrow :: TreacheryCard CenturiesOfSorrow
centuriesOfSorrow = treachery CenturiesOfSorrow Cards.centuriesOfSorrow

-- | Surge.
-- When Revealed: The first player must choose a new quest to be the current quest, if able.
-- Shadow: If the defending player does not control at least 1 Clue objective, this attack
-- is considered undefended.
instance RunMessage CenturiesOfSorrow where
  runMessage msg t@(CenturiesOfSorrow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseNewCurrentQuest lead attrs
      pure t
    ResolveShadowEffect cid eid pid (_ : _) | cid == attrs.cardId -> do
      clue <- controlsClue pid
      unless clue $ makeUndefended pid eid
      pure t
    _ -> CenturiesOfSorrow <$> runMessage msg attrs
