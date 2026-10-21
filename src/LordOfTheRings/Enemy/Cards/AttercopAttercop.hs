module LordOfTheRings.Enemy.Cards.AttercopAttercop (attercopAttercop) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype AttercopAttercop = AttercopAttercop EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

attercopAttercop :: EnemyCard AttercopAttercop
attercopAttercop = enemy AttercopAttercop Cards.attercopAttercop

-- | Forced: At the beginning of the encounter phase, Attercop, Attercop automatically
-- engages the player guarding Gollum, regardless of his threat.
instance HasAbilities AttercopAttercop where
  getAbilities (AttercopAttercop a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage AttercopAttercop where
  runMessage msg e@(AttercopAttercop attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getGuard >>= traverse_ (`engageEnemy` attrs.id)
      pure e
    _ -> AttercopAttercop <$> runMessage msg attrs
