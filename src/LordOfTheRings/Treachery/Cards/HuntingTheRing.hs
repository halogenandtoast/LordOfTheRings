module LordOfTheRings.Treachery.Cards.HuntingTheRing (huntingTheRing) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (playersAtThisStage, toFirstPlayersStagingArea)
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HuntingTheRing = HuntingTheRing TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingTheRing :: TreacheryCard HuntingTheRing
huntingTheRing = treachery HuntingTheRing Cards.huntingTheRing

instance HasModifiersFor HuntingTheRing where
  getModifiersFor (HuntingTheRing a) = withHost a (\_ -> modifySelf a [GainTrait Condition])

instance HasAbilities HuntingTheRing where
  getAbilities (HuntingTheRing a) = [mkAbility a 1 $ forced RoundEndsWindow | AttachedTo (QuestTarget _) <- [a.placement]]

instance RunMessage HuntingTheRing where
  runMessage msg t@(HuntingTheRing a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      traverse_ (attachTreachery a.id) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      players <- playersAtThisStage
      for_ players (\pid -> encounterRaisesThreat pid $ length enemies)
      doStep 1 msg
      pure t
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      -- Resolve the "then" instruction after the threat increases and their
      -- windows; move the enemies currently in this staging area.
      enemies <- select EnemyInStagingArea
      discard a a
      for_ enemies toFirstPlayersStagingArea
      pure t
    _ -> HuntingTheRing <$> runMessage msg a
