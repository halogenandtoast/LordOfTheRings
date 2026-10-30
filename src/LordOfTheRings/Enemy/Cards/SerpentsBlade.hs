module LordOfTheRings.Enemy.Cards.SerpentsBlade (serpentsBlade) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SerpentsBlade = SerpentsBlade EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

serpentsBlade :: EnemyCard SerpentsBlade
serpentsBlade = enemy SerpentsBlade Cards.serpentsBlade

-- | Forced: When the Stormcaller advances to the next quest stage, Serpent's Blade makes an
-- immediate attack against the engaged player.
instance RunMessage SerpentsBlade where
  runMessage msg e@(SerpentsBlade attrs) = case msg of
    ScenarioSpecific "stormcaller.advanced" _ | EngagedWith pid <- attrs.placement -> do
      immediateAttack pid attrs.id
      pure e
    _ -> SerpentsBlade <$> runMessage msg attrs
