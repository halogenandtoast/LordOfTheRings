module LordOfTheRings.Enemy.Cards.ShadowHostCaptain (shadowHostCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype ShadowHostCaptain = ShadowHostCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowHostCaptain :: EnemyCard ShadowHostCaptain
shadowHostCaptain = enemy ShadowHostCaptain Cards.shadowHostCaptain

instance HasModifiersFor ShadowHostCaptain where
  getModifiersFor (ShadowHostCaptain a) = do
    time <- currentNight
    let bonus = case time of Just Midnight -> 2; Just Dusk -> 1; _ -> 0
    modifySelf a [AttackModifier bonus, DefenseModifier bonus]
