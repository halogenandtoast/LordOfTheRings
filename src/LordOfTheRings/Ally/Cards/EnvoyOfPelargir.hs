module LordOfTheRings.Ally.Cards.EnvoyOfPelargir (envoyOfPelargir) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor, Noble))

newtype EnvoyOfPelargir = EnvoyOfPelargir AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

envoyOfPelargir :: AllyCard EnvoyOfPelargir
envoyOfPelargir = ally EnvoyOfPelargir Cards.envoyOfPelargir

-- | Response: After Envoy of Pelargir enters play, add 1 resource to a Gondor or Noble
-- hero's resource pool.
instance HasAbilities EnvoyOfPelargir where
  getAbilities (EnvoyOfPelargir a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage EnvoyOfPelargir where
  runMessage msg e@(EnvoyOfPelargir a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- ordNub . concat <$> traverse (select . HeroWithTrait) [Gondor, Noble]
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroForResource"
        causedBy a
        targets heroes \h -> placeResources a h 1
      pure e
    _ -> EnvoyOfPelargir <$> runMessage msg a
