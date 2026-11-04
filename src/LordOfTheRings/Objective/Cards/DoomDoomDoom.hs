module LordOfTheRings.Objective.Cards.DoomDoomDoom (doomDoomDoom) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (balrogAttacks, removeDoomDamage)

newtype DoomDoomDoom = DoomDoomDoom ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

doomDoomDoom :: ObjectiveCard DoomDoomDoom
doomDoomDoom = objective DoomDoomDoom Cards.doomDoomDoom

{- | Forced: At the end of the quest phase, remove 1 damage token from Doom, Doom, Doom.
Then, if there are no damage tokens here, add The Balrog to the staging area.
Forced: At the end of the quest phase, The Balrog makes an attack if it is in play.

The second triggers while The Balrog is in play as the phase ends; with both, the first
player orders them, as Forced effects with the same timing.
-}
instance HasAbilities DoomDoomDoom where
  getAbilities (DoomDoomDoom a) =
    [ mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase
    , restricted a 2 (exists $ EnemyWithCardCode $ toCardCode Enemies.theBalrog) $ forced $ PhaseEndsWindow When QuestPhase
    ]

instance RunMessage DoomDoomDoom where
  runMessage msg o@(DoomDoomDoom attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeDoomDamage attrs
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      damage <- getDamage (toTarget attrs)
      when (damage == 0) $ addSetAsideToStaging (cardIs Enemies.theBalrog)
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      balrogAttacks
      pure o
    _ -> DoomDoomDoom <$> runMessage msg attrs
