module LordOfTheRings.Hero.Cards.BrandSonOfBain (brandSonOfBain) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BrandSonOfBain = BrandSonOfBain HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged.
-- Response: After Brand son of Bain attacks and defeats an enemy engaged with another
-- player, choose and ready one of that player's characters.
--
-- ponytail: characters attack only enemies engaged with their own player so far, so the
-- response has no attack to follow; it comes with ranged attacks across the table.
brandSonOfBain :: HeroCard BrandSonOfBain
brandSonOfBain = hero BrandSonOfBain Cards.brandSonOfBain
