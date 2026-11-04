module LordOfTheRings.Location.Cards.ScorchedHamada (scorchedHamada) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (temperatureX)

newtype ScorchedHamada = ScorchedHamada LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

scorchedHamada :: LocationCard ScorchedHamada
scorchedHamada = location ScorchedHamada Cards.scorchedHamada

-- | X is the tens digit of the temperature.
-- Forced: After Scorched Hamada becomes the active location, assign X damage among
-- characters in play.
instance HasAbilities ScorchedHamada where
  getAbilities (ScorchedHamada a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The first player assigns the damage, as Tangled Forest's.
-- Shadow: Raise your threat by X, where X is the tens digit of the temperature.
instance RunMessage ScorchedHamada where
  runMessage msg l@(ScorchedHamada a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      x <- temperatureX
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      when (x > 0) $ dealDamageDividedAmong a lead characters x
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      x <- temperatureX
      encounterRaisesThreat pid x
      pure l
    _ -> ScorchedHamada <$> runMessage msg a
