module LordOfTheRings.Location.Cards.EnchantedLand (enchantedLand) where

import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype EnchantedLand = EnchantedLand LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

enchantedLand :: LocationCard EnchantedLand
enchantedLand = location EnchantedLand Cards.enchantedLand

instance HasAbilities EnchantedLand where
  getAbilities (EnchantedLand a) = [mkAbility a 1 $ forced $ EncounterEffectCanceledWindow After]

instance RunMessage EnchantedLand where
  runMessage msg l@(EnchantedLand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      increasePursuit 1
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      immune <- immuneToSource (toSource a) eid
      chooseOrRunOneM pid do
        questionLabeled "enchantedLand.choose"
        causedBy a
        labeled "enchantedLand.pursuit" $ increasePursuit 1
        unless immune $ labeled "enchantedLand.protect" $ roundModifier a eid CannotTakeDamage
      pure l
    _ -> EnchantedLand <$> runMessage msg a
