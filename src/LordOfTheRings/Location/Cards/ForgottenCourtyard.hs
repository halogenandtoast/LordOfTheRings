module LordOfTheRings.Location.Cards.ForgottenCourtyard (forgottenCourtyard) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

newtype ForgottenCourtyard = ForgottenCourtyard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

forgottenCourtyard :: LocationCard ForgottenCourtyard
forgottenCourtyard = location ForgottenCourtyard Cards.forgottenCourtyard

-- | Forgotten Courtyard gets +1 threat for each resource on it.
instance HasModifiersFor ForgottenCourtyard where
  getModifiersFor (ForgottenCourtyard a) = do
    let n = countTokens Resource a.locationTokens
    when (n > 0) $ modified a a [ThreatModifier n]

-- | Forced: After an Undead enemy enters play, place 1 resource on Forgotten Courtyard.
-- ponytail: as it enters the staging area; one put into play engaged is missed.
instance HasAbilities ForgottenCourtyard where
  getAbilities (ForgottenCourtyard a) = [mkAbility a 1 $ forced $ EnemyWithTraitEntersStagingAreaWindow After Undead]

instance RunMessage ForgottenCourtyard where
  runMessage msg l@(ForgottenCourtyard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure l
    _ -> ForgottenCourtyard <$> runMessage msg a
