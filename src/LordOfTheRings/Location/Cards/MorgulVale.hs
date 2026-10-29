module LordOfTheRings.Location.Cards.MorgulVale (morgulVale) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)

newtype MorgulVale = MorgulVale LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulVale :: LocationCard MorgulVale
morgulVale = location MorgulVale Cards.morgulVale

-- | Forced: At the end of the round, each player returns 1 enemy engaged with him to the
-- staging area.
instance HasAbilities MorgulVale where
  getAbilities (MorgulVale a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Shadow: If this attack destroys a character, place 1 progress token on To the Tower.
instance RunMessage MorgulVale where
  runMessage msg l@(MorgulVale a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "morgulVale.return"
          causedBy a
          targets engaged \e -> placeEnemy e StagingArea
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      placeOnTower a 1
      pure l
    _ -> MorgulVale <$> runMessage msg a
