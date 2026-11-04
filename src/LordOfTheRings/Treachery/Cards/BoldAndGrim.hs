module LordOfTheRings.Treachery.Cards.BoldAndGrim (boldAndGrim) where

import LordOfTheRings.Card (IsCard (toCard), cdUnique, toCardDef)
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (haradObjectivesControlledBy, heroesWithObjectives)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BoldAndGrim = BoldAndGrim TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boldAndGrim :: TreacheryCard BoldAndGrim
boldAndGrim = treachery BoldAndGrim Cards.boldAndGrim

{- | Surge. Doomed 1. When Revealed: Deal 1 damage to each hero with an objective attached.
Shadow: If attacking enemy is non-unique, either attach a Harad objective you control to
attacking enemy, or it makes an additional attack after this one. Attached, the objective
goes back to the staging area guarded by the enemy, as a Guarded (X) objective is.
-}
instance RunMessage BoldAndGrim where
  runMessage msg t@(BoldAndGrim attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- heroesWithObjectives
      for_ heroes \h -> dealDamage attrs h 1
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      enemy <- getEnemy eid
      unless (cdUnique $ toCardDef enemy) do
        objectives <- haradObjectivesControlledBy pid
        chooseOrRunOneM pid do
          questionLabeled "boldAndGrim.choose"
          causedBy attrs
          targets (map (.objectiveId) objectives) \o -> do
            placeObjective o StagingArea
            guardObjectiveWith o (toCard enemy).id
          -- the attack is added to the game's queue once chosen
          labeled "boldAndGrim.attack" $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> BoldAndGrim <$> runMessage msg attrs
