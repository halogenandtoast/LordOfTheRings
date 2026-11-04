module LordOfTheRings.Objective.Cards.GwaihirTheBlackGateOpens (gwaihirTheBlackGateOpens) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype GwaihirTheBlackGateOpens = GwaihirTheBlackGateOpens ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally as it is revealed, in the staging area until the first player
-- chooses; under his control, he is 'LordOfTheRings.Ally.Cards.GwaihirTheBlackGateOpens'.
gwaihirTheBlackGateOpens :: ObjectiveCard GwaihirTheBlackGateOpens
gwaihirTheBlackGateOpens = objective GwaihirTheBlackGateOpens Cards.gwaihirTheBlackGateOpens

{- | When Revealed: The first player must choose: either take control of Gwaihir, or spend 1
Fellowship resource and discard Gwaihir to discard a non-unique enemy in the staging area.
The second is offered while he has a Fellowship resource and there is such an enemy.
Shadow: This attack deals no damage: its damage is not dealt.
-}
instance RunMessage GwaihirTheBlackGateOpens where
  runMessage msg o@(GwaihirTheBlackGateOpens attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      fellowship <- getResourcesOfSphere lead Fellowship
      enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy)
      chooseOrRunOneM lead do
        questionLabeled "gwaihirTheBlackGateOpens.choose"
        causedBy attrs
        labeled "gwaihirTheBlackGateOpens.control" $ putObjectiveAllyIntoPlay attrs.id lead
        when (fellowship >= 1 && notNull enemies) $ labeled "gwaihirTheBlackGateOpens.discard" do
          spendResources lead Fellowship 1
          discard attrs
          chooseOrRunOneM lead do
            questionLabeled "gwaihirTheBlackGateOpens.discardEnemy"
            causedBy attrs
            targets enemies discard
      pure o
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      cancelMatching \case
        DefendedByMany _ e _ -> e == eid
        UndefendedDamage _ e -> e == eid
        AttackDamage e _ -> e == eid
        _ -> False
      pure o
    _ -> GwaihirTheBlackGateOpens <$> runMessage msg attrs
