module LordOfTheRings.Location.Cards.WeathertopNightmare (weathertopNightmare) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype WeathertopNightmare = WeathertopNightmare LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

weathertopNightmare :: LocationCard WeathertopNightmare
weathertopNightmare = location WeathertopNightmare Cards.weathertopNightmare

instance HasModifiersFor WeathertopNightmare where
  getModifiersFor (WeathertopNightmare a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    enemies <- select (EnemyWithCardCode $ toCardCode Enemies.theWitchKing)
    for_ enemies \eid -> modified a eid [CannotTakeDamage]
    when (a.placement == ActiveLocation)
      $ getSagaOneRing
      >>= traverse_ (\ring -> modified a ring [CannotReady])

instance HasAbilities WeathertopNightmare where
  getAbilities (WeathertopNightmare a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage WeathertopNightmare where
  runMessage msg l@(WeathertopNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      getSagaOneRing >>= traverse_ exhaust
      pure l
    _ -> WeathertopNightmare <$> runMessage msg a
