module LordOfTheRings.Hero.Cards.AragornTheFlameOfTheWest (aragornTheFlameOfTheWest) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype AragornTheFlameOfTheWest = AragornTheFlameOfTheWest HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Fellowship sphere Aragorn the first player takes control of as The Flame of the
-- West's scenarios are set up: his stats and keyword only, the player cards' own version
-- giving his text.
aragornTheFlameOfTheWest :: HeroCard AragornTheFlameOfTheWest
aragornTheFlameOfTheWest = hero AragornTheFlameOfTheWest Cards.aragornTheFlameOfTheWest

instance RunMessage AragornTheFlameOfTheWest where
  runMessage msg (AragornTheFlameOfTheWest a) = AragornTheFlameOfTheWest <$> runMessage msg a
