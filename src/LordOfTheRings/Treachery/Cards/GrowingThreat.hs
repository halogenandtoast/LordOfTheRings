module LordOfTheRings.Treachery.Cards.GrowingThreat (growingThreat) where

import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (toFirstPlayersStagingArea)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GrowingThreat = GrowingThreat TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

growingThreat :: TreacheryCard GrowingThreat
growingThreat = treachery GrowingThreat Cards.growingThreat

{- | Peril. When Revealed: Either move 1 enemy engaged with you to the first player's staging
area, or Growing Threat gains doomed 2 and surge: doomed 2 raises each player's threat by 2
as the keyword does, and surge reveals another card.
Shadow: After this attack, move attacking enemy to the first player's staging area.
-}
instance RunMessage GrowingThreat where
  runMessage msg t@(GrowingThreat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      engaged <- select (EnemyEngagedWith $ PlayerWithId you)
      chooseOrRunOneM you do
        questionLabeled "growingThreat.choose"
        causedBy attrs
        unless (null engaged) $ labeled "growingThreat.move" $ chooseOrRunOneM you do
          questionLabeled "growingThreat.chooseEnemy"
          causedBy attrs
          targets engaged toFirstPlayersStagingArea
        labeled "growingThreat.doomed" do
          raiseThreatFromDoomed 2
          revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      lead <- getLead
      afterAttack eid [PlaceInStagingAreaOf lead (EnemyTarget eid)]
      pure t
    _ -> GrowingThreat <$> runMessage msg attrs
