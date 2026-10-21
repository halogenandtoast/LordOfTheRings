module LordOfTheRings.Objective.Cards.GollumReturnToMirkwood (gollumReturnToMirkwood) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (passGollum)

newtype GollumReturnToMirkwood = GollumReturnToMirkwood ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumReturnToMirkwood :: ObjectiveCard GollumReturnToMirkwood
gollumReturnToMirkwood = objective GollumReturnToMirkwood Cards.gollumReturnToMirkwood

-- | Damage from undefended attacks against you must be dealt to Gollum.
instance HasModifiersFor GollumReturnToMirkwood where
  getModifiersFor (GollumReturnToMirkwood a) = case a.placement of
    InPlayArea guard' -> modified a (PlayerTarget guard') [UndefendedDamageTo (toTarget a)]
    _ -> pure ()

-- | Forced: At the end of each round, raise the threat of the player guarding Gollum by 3.
-- Then, that player may choose a new player to guard Gollum.
instance HasAbilities GollumReturnToMirkwood where
  getAbilities (GollumReturnToMirkwood a) = [mkAbility a 1 $ forced RoundEndsWindow | InPlayArea _ <- [a.placement]]

-- | If Gollum is destroyed, or if the player guarding Gollum is eliminated, the players
-- have lost the game.
instance RunMessage GollumReturnToMirkwood where
  runMessage msg o@(GollumReturnToMirkwood attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (guardOf attrs) \g -> encounterRaisesThreat g 3
      passGollum True
      pure o
    PlaceTokens _ (isTarget attrs -> True) Damage n | countTokens Damage attrs.objectiveTokens + n >= 5 -> do
      gameOver Lost
      GollumReturnToMirkwood <$> runMessage msg attrs
    EliminatePlayer pid | guardOf attrs == Just pid -> do
      gameOver Lost
      pure o
    _ -> GollumReturnToMirkwood <$> runMessage msg attrs
   where
    guardOf a = case a.placement of
      InPlayArea pid -> Just pid
      _ -> Nothing
