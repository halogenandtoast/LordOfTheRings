module LordOfTheRings.Location.Cards.DunlandBattlefield (dunlandBattlefield) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (assignDamage)

newtype DunlandBattlefield = DunlandBattlefield LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlandBattlefield :: LocationCard DunlandBattlefield
dunlandBattlefield = location DunlandBattlefield Cards.dunlandBattlefield

-- | Forced: After the last time counter is removed from this location, each player assigns
-- X damage among characters he controls, where X is the number of cards in his hand.
instance HasAbilities DunlandBattlefield where
  getAbilities (DunlandBattlefield a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Shadow: Deal 1 damage to the defending character.
instance RunMessage DunlandBattlefield where
  runMessage msg l@(DunlandBattlefield a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        hand <- length . (.hand) <$> getPlayer pid
        assignDamage a pid hand
      pure l
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> dealDamage a d 1
      pure l
    _ -> DunlandBattlefield <$> runMessage msg a
