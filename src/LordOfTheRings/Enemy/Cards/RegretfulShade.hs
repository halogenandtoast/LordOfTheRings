module LordOfTheRings.Enemy.Cards.RegretfulShade (regretfulShade) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype RegretfulShade = RegretfulShade EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

regretfulShade :: EnemyCard RegretfulShade
regretfulShade = enemy RegretfulShade Cards.regretfulShade

instance RunMessage RegretfulShade where
  runMessage msg e@(RegretfulShade a) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      time <- currentNight
      let additional = case time of Just Midnight -> 3; Just Dusk -> 2; _ -> 1
      dealShadowCardsDuringAttack eid pid defenders additional
      pure e
    _ -> RegretfulShade <$> runMessage msg a
