module LordOfTheRings.Location.Cards.SarnGebir (sarnGebir) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SarnGebir = SarnGebir LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

sarnGebir :: LocationCard SarnGebir
sarnGebir = location SarnGebir Cards.sarnGebir

-- | Immune to player card effects.
instance HasModifiersFor SarnGebir where
  getModifiersFor (SarnGebir a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: When Sarn Gebir is explored, deal 1 damage to each exhausted character.
instance HasAbilities SarnGebir where
  getAbilities (SarnGebir a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage SarnGebir where
  runMessage msg l@(SarnGebir a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage a c 1
      pure l
    _ -> SarnGebir <$> runMessage msg a
