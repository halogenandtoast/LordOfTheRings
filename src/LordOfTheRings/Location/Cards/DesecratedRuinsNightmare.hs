module LordOfTheRings.Location.Cards.DesecratedRuinsNightmare (desecratedRuinsNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | Uncharted, under an Edge of the Temple side.
newtype DesecratedRuinsNightmare = DesecratedRuinsNightmare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

desecratedRuinsNightmare :: LocationCard DesecratedRuinsNightmare
desecratedRuinsNightmare = location DesecratedRuinsNightmare Cards.desecratedRuinsNightmare

-- | Forced: When Desecrated Ruins is flipped over while active, remove all resources from
-- each hero and objective-ally.
instance HasAbilities DesecratedRuinsNightmare where
  getAbilities (DesecratedRuinsNightmare a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage DesecratedRuinsNightmare where
  runMessage msg l@(DesecratedRuinsNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      heroes <- traverse getHero =<< select AnyHero
      for_ heroes \h -> when (h.resources > 0) $ removeTokens a h.id Resource h.resources
      allies <- traverse getAlly =<< select ObjectiveAlly
      for_ allies \x -> when (x.resources > 0) $ removeTokens a x.id Resource x.resources
      pure l
    _ -> DesecratedRuinsNightmare <$> runMessage msg a
