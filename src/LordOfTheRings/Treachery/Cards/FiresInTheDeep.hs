module LordOfTheRings.Treachery.Cards.FiresInTheDeep (firesInTheDeep) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FiresInTheDeep = FiresInTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

firesInTheDeep :: TreacheryCard FiresInTheDeep
firesInTheDeep = treachery FiresInTheDeep Cards.firesInTheDeep

-- | When Revealed: Each player must discard 1 ally he controls, if able. If Fires in the
-- Deep discarded no allies, it gains surge.
-- Shadow: The defending player discards 1 ally he controls, if able.
instance RunMessage FiresInTheDeep where
  runMessage msg t@(FiresInTheDeep attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discarded <- for players \p -> do
        allies <- alliesOf p
        chooseAndDiscard attrs p allies
        pure $ notNull allies
      unless (or discarded) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndDiscard attrs pid =<< alliesOf pid
      pure t
    _ -> FiresInTheDeep <$> runMessage msg attrs

alliesOf :: HasGame m => PlayerId -> m [Target]
alliesOf p = map toTarget <$> select (AllyControlledBy $ PlayerWithId p)
