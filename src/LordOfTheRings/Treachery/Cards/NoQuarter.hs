module LordOfTheRings.Treachery.Cards.NoQuarter (noQuarter) where

import LordOfTheRings.Card (IsCard (toCard), cdUnique, toCardDef)
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (haradObjectives, haradObjectivesControlledBy)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NoQuarter = NoQuarter TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

noQuarter :: TreacheryCard NoQuarter
noQuarter = treachery NoQuarter Cards.noQuarter

{- | When Revealed: Either reveal 2 encounter cards, or detach a Harad objective from a
hero, reveal it, and add it to the staging area. The first player chooses; revealed, the
objective's Guarded (X) finds it a new guard.
Shadow: If this attack destroys a character, attach a Harad objective you control to a
non-unique encounter card in the staging area.
-}
instance RunMessage NoQuarter where
  runMessage msg t@(NoQuarter attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      onHeroes <- filter (\o -> case o.placement of AttachedTo (HeroTarget _) -> True; _ -> False) <$> haradObjectives
      chooseOrRunOneM lead do
        questionLabeled "noQuarter.choose"
        causedBy attrs
        labeled "noQuarter.reveal" $ revealEncounterCards 2
        targets (map (.objectiveId) onHeroes) \o -> do
          placeObjective o StagingArea
          resolveGuarded o
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      objectives <- haradObjectivesControlledBy pid
      enemies <- filter (not . cdUnique . toCardDef) <$> (traverse getEnemy =<< select EnemyInStagingArea)
      locations <- filter (not . cdUnique . toCardDef) <$> (traverse getLocation =<< select LocationInStagingArea)
      let hosts = [(toTarget e, (toCard e).id) | e <- enemies] <> [(toTarget l, (toCard l).id) | l <- locations]
      unless (null hosts) $ chooseOrRunOneM pid do
        questionLabeled "noQuarter.objective"
        causedBy attrs
        targets (map (.objectiveId) objectives) \o ->
          chooseOrRunOneM pid do
            questionLabeled "noQuarter.host"
            causedBy attrs
            for_ hosts \(host, hostCard) -> targeting host do
              placeObjective o StagingArea
              guardObjectiveWith o hostCard
      pure t
    _ -> NoQuarter <$> runMessage msg attrs
