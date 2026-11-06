module LordOfTheRings.Location.Cards.ShadowOfDwimorberg (shadowOfDwimorberg) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype ShadowOfDwimorberg = ShadowOfDwimorberg LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowOfDwimorberg :: LocationCard ShadowOfDwimorberg
shadowOfDwimorberg = location ShadowOfDwimorberg Cards.shadowOfDwimorberg

instance HasModifiersFor ShadowOfDwimorberg where
  getModifiersFor (ShadowOfDwimorberg a) = do
    time <- currentNight
    let bonus = case time of Just Midnight -> 4; Just Dusk -> 2; _ -> 0
    modifySelf a [ThreatModifier bonus]
