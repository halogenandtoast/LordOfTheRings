module LordOfTheRings.Ally.Cards.CaptiveVillager (captiveVillager) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))

newtype CaptiveVillager = CaptiveVillager AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, free of encounters under the first player's control.
captiveVillager :: AllyCard CaptiveVillager
captiveVillager = ally CaptiveVillager Cards.captiveVillager

-- | Its controller is considered to have +5 threat while making engagement checks.
instance HasModifiersFor CaptiveVillager where
  getModifiersFor (CaptiveVillager a) = for_ [p | InPlayArea p <- [a.placement]] \pid ->
    modified a (PlayerTarget pid) [ThreatForEngagementChecks 5]

-- | If a Captive Villager leaves play, the players lose the game (Nightmare setup card).
instance HasAbilities CaptiveVillager where
  getAbilities (CaptiveVillager a) = [mkAbility a 1 $ forced $ LeftPlayWindow After (toTarget a)]

instance RunMessage CaptiveVillager where
  runMessage msg s@(CaptiveVillager a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      gameOver Lost
      pure s
    _ -> CaptiveVillager <$> runMessage msg a
