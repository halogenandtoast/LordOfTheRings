module LordOfTheRings.Ally.Cards.LordAlcaron (lordAlcaron) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (alcaronMayRedirect)

newtype LordAlcaron = LordAlcaron AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, under the first player's control from the start.
lordAlcaron :: AllyCard LordAlcaron
lordAlcaron = ally LordAlcaron Cards.lordAlcaron

-- | Response: After a villager token is discarded, exhaust Lord Alcaron to place that
-- villager token on a location instead.
-- If Lord Alcaron leaves play, the players have lost the game.
-- The Response is asked as each token is discarded, in place of a response window;
-- no other card answers a discarded villager
instance RunMessage LordAlcaron where
  runMessage msg s@(LordAlcaron a) = case msg of
    DoStep 1 (RemoveTokens source target Villager n) -> do
      may <- alcaronMayRedirect
      for_ (guard may *> placementController a.placement) \pid -> do
        locations <- select AnyLocation
        chooseOneM pid do
          questionLabeled "lordAlcaron.placeVillager"
          causedBy a
          targets locations \l -> do
            exhaust a
            placeTokens a l Villager 1
          labeled "lordAlcaron.decline" $ doStep 2 (RemoveTokens source target Villager n)
      pure s
    Discard _ (isTarget a -> True) -> do
      gameOver Lost
      pure s
    _ -> LordAlcaron <$> runMessage msg a
