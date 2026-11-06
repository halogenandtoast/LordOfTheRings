module LordOfTheRings.Objective.Cards.RelicFromTheDarkYears (relicFromTheDarkYears) where

import LordOfTheRings.Keyword (Keyword (Spectral))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)
import LordOfTheRings.Trait (Trait (Oathbreaker, Item, Artifact))

newtype RelicFromTheDarkYears = RelicFromTheDarkYears ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

relicFromTheDarkYears :: ObjectiveCard RelicFromTheDarkYears
relicFromTheDarkYears = objective RelicFromTheDarkYears Cards.relicFromTheDarkYears

instance HasModifiersFor RelicFromTheDarkYears where
  getModifiersFor (RelicFromTheDarkYears a) = when (a.placement == StagingArea) do
    time <- currentNight
    when (time /= Just Midnight) $ modifySelf a [CannotTriggerAbilities]

instance HasAbilities RelicFromTheDarkYears where
  getAbilities (RelicFromTheDarkYears a) = case a.placement of
    StagingArea ->
      [anyPlayer $ restricted a 1 (exists $ HeroControlledBy ActivePlayer <> relicHost) $ ActionAbility Free]
    AttachedTo host@(HeroTarget _) ->
      [mkAbility a 2 $ silent $ AttackerDeclaredWindow After (Just host)]
    _ -> []

instance RunMessage RelicFromTheDarkYears where
  runMessage msg o@(RelicFromTheDarkYears a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select $ HeroControlledBy (PlayerWithId pid) <> relicHost
      chooseOneM pid do
        questionLabeled "erech.claimRelic"
        causedBy a
        targets heroes $ attachObjective a.id
      pure o
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      for_ [eid | Window _ (DeclaredAsAttacker _ eid) <- windows] \eid ->
        whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Oathbreaker) do
          phaseModifier a eid (LoseKeyword Spectral)
      pure o
    _ -> RelicFromTheDarkYears <$> runMessage msg a

relicHost :: HeroMatcher
relicHost = HeroCanHaveEncounterAttachment (setFromList [Item, Artifact])
