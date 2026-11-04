module LordOfTheRings.Enemy.Cards.FellRider (fellRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)
import LordOfTheRings.Trait (Trait (Morgul))

newtype FellRider = FellRider EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

fellRider :: EnemyCard FellRider
fellRider = enemy FellRider Cards.fellRider

-- | Cannot have non-Morgul attachments.
instance HasModifiersFor FellRider where
  getModifiersFor (FellRider a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

-- | While Fell Rider is in the staging area, it gains: "Forced: At the beginning of the
-- encounter phase, the first player must either reduce the Ring-bearer's life by 1 or
-- engage Fell Rider."
instance HasAbilities FellRider where
  getAbilities (FellRider a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase | a.placement == StagingArea]

instance RunMessage FellRider where
  runMessage msg e@(FellRider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "fellRider.choose"
        causedBy attrs
        labeled "fellRider.reduceLife" $ reduceRingBearersLife 1
        labeled "fellRider.engage" $ engageEnemy lead attrs.id
      pure e
    _ -> FellRider <$> runMessage msg attrs
