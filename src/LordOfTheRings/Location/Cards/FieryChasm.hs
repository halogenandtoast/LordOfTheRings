module LordOfTheRings.Location.Cards.FieryChasm (fieryChasm) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains.Helpers (ashX)

newtype FieryChasm = FieryChasm LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

fieryChasm :: LocationCard FieryChasm
fieryChasm = location FieryChasm Cards.fieryChasm

-- | X is 1 more than the number of quest cards in play.
instance HasModifiersFor FieryChasm where
  getModifiersFor (FieryChasm a) = do
    x <- ashX
    modifySelf a [ThreatModifier x]

-- | Forced: When Fiery Chasm is explored, discard the top X cards of your deck. No player
-- revealed or travels for the exploring, so "you" is the first player, who resolves the
-- encounter cards' effects that name no other.
instance HasAbilities FieryChasm where
  getAbilities (FieryChasm a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage FieryChasm where
  runMessage msg l@(FieryChasm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      x <- ashX
      discardTopOfDeck lead x
      pure l
    _ -> FieryChasm <$> runMessage msg attrs
