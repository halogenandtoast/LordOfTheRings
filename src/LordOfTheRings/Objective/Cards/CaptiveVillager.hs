module LordOfTheRings.Objective.Cards.CaptiveVillager (captiveVillager) where

import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (guardFromOrcDeck)

newtype CaptiveVillager = CaptiveVillager ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, guarded in the staging area; free, it is an ally of the first
-- player's (Ally.Cards.CaptiveVillager).
captiveVillager :: ObjectiveCard CaptiveVillager
captiveVillager = objective CaptiveVillager Cards.captiveVillager

-- | Forced: After Captive Villager enters play, reveal the top card of the Orc deck and
-- attach it to Captive Villager, guarding it.
instance HasAbilities CaptiveVillager where
  getAbilities (CaptiveVillager a) = [mkAbility a 1 $ forced $ CardEntersStagingAreaWindow After (toTarget a)]

-- | While Captive Villager is free of encounters, the first player gains control of it:
-- checked as each window opens, once its guard has left play.
-- If a Captive Villager leaves play, the players lose the game (Nightmare setup card).
instance RunMessage CaptiveVillager where
  runMessage msg o@(CaptiveVillager attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      guardFromOrcDeck attrs.id
      pure o
    CheckWindows _ | Just cid <- attrs.objectiveGuard -> do
      es <- getsGame gameEntities
      let inPlay = any ((== cid) . (.id) . toCard) (toList es.enemies) || any ((== cid) . (.id) . toCard) (toList es.locations)
      if inPlay
        then CaptiveVillager <$> runMessage msg attrs
        else do
          putObjectiveAllyIntoPlay attrs.id =<< getLead
          CaptiveVillager <$> runMessage msg (attrs & guardL .~ Nothing)
    Discard (isTarget attrs -> True) -> do
      gameOver Lost
      pure o
    _ -> CaptiveVillager <$> runMessage msg attrs
