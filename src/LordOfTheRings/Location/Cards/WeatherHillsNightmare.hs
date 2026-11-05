module LordOfTheRings.Location.Cards.WeatherHillsNightmare (weatherHillsNightmare) where

import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype WeatherHillsNightmare = WeatherHillsNightmare LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

weatherHillsNightmare :: LocationCard WeatherHillsNightmare
weatherHillsNightmare = location WeatherHillsNightmare Cards.weatherHillsNightmare

instance HasModifiersFor WeatherHillsNightmare where
  getModifiersFor (WeatherHillsNightmare a) = do
    firstStage <- any ((== 1) . (.stage)) <$> getMainQuestCards
    exhausted <- theOneRingExhausted
    modifySelf a $ [ProgressLimit 0 | firstStage] <> [ThreatModifier 2 | exhausted]

instance RunMessage WeatherHillsNightmare where
  runMessage msg l@(WeatherHillsNightmare a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 2
      pure l
    _ -> WeatherHillsNightmare <$> runMessage msg a
