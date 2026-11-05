module LordOfTheRings.Location.Cards.FordOfBruinenNightmare (fordOfBruinenNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Nazgul))

newtype FordOfBruinenNightmare = FordOfBruinenNightmare LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fordOfBruinenNightmare :: LocationCard FordOfBruinenNightmare
fordOfBruinenNightmare = location FordOfBruinenNightmare Cards.fordOfBruinenNightmare

instance HasModifiersFor FordOfBruinenNightmare where
  getModifiersFor (FordOfBruinenNightmare a) = modifySelf a [ImmuneToPlayerCardEffects]

instance RunMessage FordOfBruinenNightmare where
  runMessage msg (FordOfBruinenNightmare a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscardToEngage pid a (CardWithType EnemyType <> CardWithTrait Nazgul)
      FordOfBruinenNightmare <$> runMessage msg a
    _ -> FordOfBruinenNightmare <$> runMessage msg a
