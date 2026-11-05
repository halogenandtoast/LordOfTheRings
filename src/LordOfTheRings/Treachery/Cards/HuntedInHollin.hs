module LordOfTheRings.Treachery.Cards.HuntedInHollin (huntedInHollin) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HuntedInHollin = HuntedInHollin TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

huntedInHollin :: TreacheryCard HuntedInHollin
huntedInHollin = treachery HuntedInHollin Cards.huntedInHollin

instance RunMessage HuntedInHollin where
  runMessage msg t@(HuntedInHollin a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      enemies <- select EnemyInStagingArea
      costs <- traverse (\eid -> (eid,) <$> getEngagementCost eid) enemies
      let lowest = minimumMay (map snd costs)
      eligible <-
        filterM
          ( \eid -> do
              mods <- getModifiers eid
              pure
                $ CannotLeaveStagingArea
                `notElem` mods
                && CannotEngagePlayer you
                `notElem` mods
                && all (== you) [p | EngagesOnly p <- mods]
          )
          [eid | (eid, n) <- costs, Just n == lowest]
      chooseOrRunOneM you do
        questionLabeled "huntedInHollin.choose"
        causedBy a
        labeled "huntedInHollin.threat" $ encounterRaisesThreat you 6
        when (notNull eligible) $ labeled "huntedInHollin.engage" do
          chooseOrRunOneM you do
            questionLabeled "huntedInHollin.enemy"
            causedBy a
            targets eligible $ \eid -> engageEnemy you eid
          revealEncounterCard
      pure t
    _ -> HuntedInHollin <$> runMessage msg a
