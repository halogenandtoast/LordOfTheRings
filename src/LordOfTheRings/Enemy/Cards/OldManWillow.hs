module LordOfTheRings.Enemy.Cards.OldManWillow (oldManWillow) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (discardRandomVictoryLocation, locationVictoryPoints)

newtype OldManWillow = OldManWillow EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

oldManWillow :: EnemyCard OldManWillow
oldManWillow = enemy OldManWillow Cards.oldManWillow

instance HasModifiersFor OldManWillow where
  getModifiersFor (OldManWillow a) = modifySelf a [ImmuneToPlayerCardEffects]

instance HasAbilities OldManWillow where
  getAbilities (OldManWillow a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage OldManWillow where
  runMessage msg e@(OldManWillow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- windows] \pid -> do
        selected <- discardRandomVictoryLocation
        for_ selected \card -> encounterRaisesThreat pid =<< locationVictoryPoints card
      pure e
    _ -> OldManWillow <$> runMessage msg a
