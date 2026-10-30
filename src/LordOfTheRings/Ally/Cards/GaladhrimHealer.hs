module LordOfTheRings.Ally.Cards.GaladhrimHealer (galadhrimHealer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GaladhrimHealer = GaladhrimHealer AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadhrimHealer :: AllyCard GaladhrimHealer
galadhrimHealer = ally GaladhrimHealer Cards.galadhrimHealer

-- | Response: After Galadhrim Healer enters play, choose a player. Heal 1 damage from each
-- hero controlled by that player.
instance HasAbilities GaladhrimHealer where
  getAbilities (GaladhrimHealer a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage GaladhrimHealer where
  runMessage msg s@(GaladhrimHealer a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        for_ heroes \h -> healDamage a h 1
      pure s
    _ -> GaladhrimHealer <$> runMessage msg a
