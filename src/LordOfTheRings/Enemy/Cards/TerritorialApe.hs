module LordOfTheRings.Enemy.Cards.TerritorialApe (territorialApe) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (locationExploredThisRound)

newtype TerritorialApe = TerritorialApe EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

territorialApe :: EnemyCard TerritorialApe
territorialApe = enemy TerritorialApe Cards.territorialApe

-- | Territorial Ape gets +X threat and +X attack, where X is the threat of the active
-- location.
instance HasModifiersFor TerritorialApe where
  getModifiersFor (TerritorialApe a) = do
    x <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
    when (x > 0) $ modifySelf a [ThreatModifier x, AttackModifier x]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if a location was explored
-- this round).
instance RunMessage TerritorialApe where
  runMessage msg e@(TerritorialApe attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      explored <- locationExploredThisRound
      enemyGetsAttack attrs eid (if explored then 2 else 1)
      pure e
    _ -> TerritorialApe <$> runMessage msg attrs
