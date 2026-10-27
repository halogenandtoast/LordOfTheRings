module LordOfTheRings.Treachery.Cards.HiddenThreat (hiddenThreat) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HiddenThreat = HiddenThreat TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hiddenThreat :: TreacheryCard HiddenThreat
hiddenThreat = treachery HiddenThreat Cards.hiddenThreat

-- | When Revealed: Each player must raise his threat by 1 for each enemy in the staging
-- area. Then, the last player discards an attachment he controls.
instance RunMessage HiddenThreat where
  runMessage msg t@(HiddenThreat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- length <$> select EnemyInStagingArea
      eachPlayer (`encounterRaisesThreat` enemies)
      lastPlayer <- getLastPlayer
      chooseAndDiscard attrs lastPlayer =<< getAttachmentsControlledBy lastPlayer
      pure t
    _ -> HiddenThreat <$> runMessage msg attrs
