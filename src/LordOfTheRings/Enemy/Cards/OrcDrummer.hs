module LordOfTheRings.Enemy.Cards.OrcDrummer (orcDrummer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OrcDrummer = OrcDrummer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcDrummer :: EnemyCard OrcDrummer
orcDrummer = enemy OrcDrummer Cards.orcDrummer

-- | While Orc Drummer is in the staging area, each enemy gets +X threat. X is the number of
-- players in the game.
instance HasModifiersFor OrcDrummer where
  getModifiersFor (OrcDrummer a) = when (a.placement == StagingArea) do
    players <- length <$> getRemainingPlayers
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [ThreatModifier players]
