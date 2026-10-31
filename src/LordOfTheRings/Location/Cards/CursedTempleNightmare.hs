module LordOfTheRings.Location.Cards.CursedTempleNightmare (cursedTempleOfTheDeceived) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under an Edge of the Temple side.
newtype CursedTempleNightmare = CursedTempleNightmare LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedTempleOfTheDeceived :: LocationCard CursedTempleNightmare
cursedTempleOfTheDeceived = location CursedTempleNightmare Cards.cursedTempleOfTheDeceived

-- | While Cursed Temple is the active location, each player cannot draw cards by card
-- effects.
instance HasModifiersFor CursedTempleNightmare where
  getModifiersFor (CursedTempleNightmare a) = when (a.placement == ActiveLocation) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotDrawCardsFromPlayerCardEffects]

-- | Forced: When Cursed Temple is flipped over while active, discard 3 cards at random from
-- each player's hand.
instance HasAbilities CursedTempleNightmare where
  getAbilities (CursedTempleNightmare a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage CursedTempleNightmare where
  runMessage msg l@(CursedTempleNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> discardRandomFromHand p 3
      pure l
    _ -> CursedTempleNightmare <$> runMessage msg a
