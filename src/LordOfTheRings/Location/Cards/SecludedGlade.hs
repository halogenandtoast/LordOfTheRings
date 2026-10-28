module LordOfTheRings.Location.Cards.SecludedGlade (secludedGlade) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SecludedGlade = SecludedGlade LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

secludedGlade :: LocationCard SecludedGlade
secludedGlade = location SecludedGlade Cards.secludedGlade

-- | Immune to player card effects.
instance HasModifiersFor SecludedGlade where
  getModifiersFor (SecludedGlade a) = modified a a [ImmuneToPlayerCardEffects]

-- | Shadow: Remove X progress tokens from the current quest, where X is the attacking
-- enemy's threat.
instance RunMessage SecludedGlade where
  runMessage msg l@(SecludedGlade a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      removeProgressFromQuest a =<< getEnemyThreat eid
      pure l
    _ -> SecludedGlade <$> runMessage msg a
