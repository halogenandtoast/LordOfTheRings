module LordOfTheRings.Location.Cards.FingerOfGlanduin (fingerOfGlanduin) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FingerOfGlanduin = FingerOfGlanduin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fingerOfGlanduin :: LocationCard FingerOfGlanduin
fingerOfGlanduin = location FingerOfGlanduin Cards.fingerOfGlanduin

-- | While Finger of Glanduin is in the staging area, it gains: Forced: At the end of each
-- round, remove 1 progress from each location in play.
instance HasAbilities FingerOfGlanduin where
  getAbilities (FingerOfGlanduin a) =
    [restricted a 1 (exists $ LocationWithId a.id <> LocationInStagingArea) $ forced RoundEndsWindow]

-- | Shadow: If this attack destroys a character remove all progress from the current
-- quest.
instance RunMessage FingerOfGlanduin where
  runMessage msg l@(FingerOfGlanduin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      locations <- getsGame (toList . (.entities.locations))
      for_ locations \loc -> when (loc.progress > 0) $ removeTokens a loc Progress 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    -- the quest's progress is all removed, however much it has
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      removeProgressFromQuest a maxBound
      pure l
    _ -> FingerOfGlanduin <$> runMessage msg a
