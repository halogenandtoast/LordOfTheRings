module LordOfTheRings.Enemy.Cards.Whisperer (whisperer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype Whisperer = Whisperer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

whisperer :: EnemyCard Whisperer
whisperer = enemy Whisperer Cards.whisperer

instance HasModifiersFor Whisperer where
  getModifiersFor (Whisperer a) = do
    time <- currentNight
    case time of
      Just Midnight -> modifySelf a [ThreatModifier 2, GainKeyword $ Doomed 2]
      Just Dusk -> modifySelf a [ThreatModifier 1]
      _ -> pure ()
