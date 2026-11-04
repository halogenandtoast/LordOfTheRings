module LordOfTheRings.Location.Cards.FordOfBruinen (fordOfBruinen) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FordOfBruinen = FordOfBruinen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

fordOfBruinen :: LocationCard FordOfBruinen
fordOfBruinen = location FordOfBruinen Cards.fordOfBruinen

-- | Immune to player card effects.
instance HasModifiersFor FordOfBruinen where
  getModifiersFor (FordOfBruinen a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Response: After Ford of Bruinen leaves play as an explored location, all engaged
-- enemies are discarded.
instance HasAbilities FordOfBruinen where
  getAbilities (FordOfBruinen a) = [mkAbility a 1 $ response $ ExploredWindow After a.id]

-- | Travel: The first player must reveal the top card of the encounter deck to travel here.
instance RunMessage FordOfBruinen where
  runMessage msg l@(FordOfBruinen a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      FordOfBruinen <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      for_ engaged discard
      pure l
    _ -> FordOfBruinen <$> runMessage msg a
