module LordOfTheRings.Hero.Cards.ThorinOakenshield (thorinOakenshield) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype ThorinOakenshield = ThorinOakenshield HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

thorinOakenshield :: HeroCard ThorinOakenshield
thorinOakenshield = hero ThorinOakenshield Cards.thorinOakenshield

-- | If you control at least 5 Dwarf characters, add 1 additional resource to Thorin
-- Oakenshield's pool when you collect resources during the resource phase.
instance HasModifiersFor ThorinOakenshield where
  getModifiersFor (ThorinOakenshield a) = do
    dwarves <- dwarfCount a.heroOwner
    when (dwarves >= 5) $ modifySelf a [CollectsAdditionalResources 1]

instance RunMessage ThorinOakenshield where
  runMessage msg (ThorinOakenshield attrs) = ThorinOakenshield <$> runMessage msg attrs
