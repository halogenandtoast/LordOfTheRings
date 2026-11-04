module LordOfTheRings.Treachery.Cards.PatrollingTheStreets (patrollingTheStreets) where

import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (ulfastX)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PatrollingTheStreets = PatrollingTheStreets TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

patrollingTheStreets :: TreacheryCard PatrollingTheStreets
patrollingTheStreets = treachery PatrollingTheStreets Cards.patrollingTheStreets

{- | When Revealed: Place 1 resource on the main quest. Then, either progress cannot be
placed on the current quest this phase, or raise each player's threat by X, where X is 1
more than the number of resources on the main quest. The first player chooses, X counted
once the resource is placed.
Shadow: Attacking enemy gets +1 Attack for each resource on the main quest.
-}
instance RunMessage PatrollingTheStreets where
  runMessage msg t@(PatrollingTheStreets attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeResourcesOnMainQuest attrs 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      lead <- getLead
      x <- ulfastX
      current <- getCurrentQuest
      chooseOneM lead do
        questionLabeled "patrollingTheStreets.choose"
        causedBy attrs
        labeled "patrollingTheStreets.noProgress" $ for_ current \q -> phaseModifier attrs q (ProgressLimit 0)
        labeledWith "patrollingTheStreets.threat" [intVar "n" x] $ eachPlayer \p -> encounterRaisesThreat p x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- getMainQuestResources
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure t
    _ -> PatrollingTheStreets <$> runMessage msg attrs
