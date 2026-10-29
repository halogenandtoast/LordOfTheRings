module LordOfTheRings.Location.Cards.FenOfReeds (fenOfReeds) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FenOfReeds = FenOfReeds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fenOfReeds :: LocationCard FenOfReeds
fenOfReeds = location FenOfReeds Cards.fenOfReeds

-- | While Fen of Reeds is in the staging area, it gains: Forced: After the players advance
-- to a quest stage, each player must exhaust a character he controls.
instance HasAbilities FenOfReeds where
  getAbilities (FenOfReeds a) =
    [restricted a 1 (exists $ LocationWithId a.id <> LocationInStagingArea) $ forced $ AdvancedToStageWindow After]

-- | Shadow: Attacking enemy gets +1 attack for each time counter on the quest.
instance RunMessage FenOfReeds where
  runMessage msg l@(FenOfReeds a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> chooseAndExhaust a p 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      counters <- maybe (pure 0) getTimeCounters =<< getCurrentQuest
      when (counters > 0) $ enemyGetsAttack a eid counters
      pure l
    _ -> FenOfReeds <$> runMessage msg a
