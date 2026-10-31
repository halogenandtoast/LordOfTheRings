module LordOfTheRings.Objective.Cards.TheCabalsChampion (theCabalsChampion) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Villain))

newtype TheCabalsChampion = TheCabalsChampion ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCabalsChampion :: ObjectiveCard TheCabalsChampion
theCabalsChampion = objective TheCabalsChampion Cards.theCabalsChampion

-- | Counts as a Title attachment with the text: "Attached enemy gets +1 threat, +1 attack,
-- +1 defense, and +2 hit points."
instance HasModifiersFor TheCabalsChampion where
  getModifiersFor (TheCabalsChampion a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, HitPointModifier 2]
    _ -> pure ()

-- | Surge. Doomed 1.
-- If The Cabal's Champion is unattached and in the staging area, attach it to a Villain
-- enemy, if able: checked as each window opens, the first player picking the Villain.
instance RunMessage TheCabalsChampion where
  runMessage msg (TheCabalsChampion attrs) = case msg of
    CheckWindows _ | attrs.placement == StagingArea -> do
      villains <- select (EnemyWithTrait Villain)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theCabalsChampion.attach"
        causedBy attrs
        targets villains $ attachObjective attrs.id
      TheCabalsChampion <$> runMessage msg attrs
    _ -> TheCabalsChampion <$> runMessage msg attrs
