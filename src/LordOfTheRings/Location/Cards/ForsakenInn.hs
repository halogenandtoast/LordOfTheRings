module LordOfTheRings.Location.Cards.ForsakenInn (forsakenInn) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Spy))

newtype ForsakenInn = ForsakenInn LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

forsakenInn :: LocationCard ForsakenInn
forsakenInn = location ForsakenInn Cards.forsakenInn

instance HasModifiersFor ForsakenInn where
  getModifiersFor (ForsakenInn a) = do
    lead <- getLead
    characters <-
      selectCharacters (HeroControlledBy $ PlayerWithId lead) (AllyControlledBy $ PlayerWithId lead)
    modifySelf a [ThreatModifier $ length characters]

instance RunMessage ForsakenInn where
  runMessage msg (ForsakenInn a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      asRevealer lead
        $ searchEncounterDeckAndDiscardToReveal lead a (CardWithType EnemyType <> CardWithTrait Spy)
      ForsakenInn <$> runMessage msg a
    _ -> ForsakenInn <$> runMessage msg a
