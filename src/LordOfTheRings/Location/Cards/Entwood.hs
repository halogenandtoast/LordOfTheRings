module LordOfTheRings.Location.Cards.Entwood (entwood) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Huorn))

newtype Entwood = Entwood LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

entwood :: LocationCard Entwood
entwood = location Entwood Cards.entwood

-- | While Entwood is in the staging area, each Huorn enemy in the staging area gains:
-- "Immune to player card effects."
instance HasModifiersFor Entwood where
  getModifiersFor (Entwood a) = when (a.placement == StagingArea) do
    huorns <- select (EnemyWithTrait Huorn <> EnemyInStagingArea)
    for_ huorns \e -> modified a e [ImmuneToPlayerCardEffects]

-- | Travel: The first player engages the highest engagement cost Huorn enemy in the staging
-- area, choosing among those tied.
instance RunMessage Entwood where
  runMessage msg l@(Entwood a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      huorns <- traverse (\e -> (e,) <$> getEngagementCost e) =<< select (EnemyWithTrait Huorn <> EnemyInStagingArea)
      let highest = foldr (max . snd) 0 huorns
      chooseOrRunOneM lead do
        questionLabeled "entwood.engage"
        causedBy a
        targets [e | (e, cost) <- huorns, cost == highest] (engageEnemy lead)
      pure l
    _ -> Entwood <$> runMessage msg a
