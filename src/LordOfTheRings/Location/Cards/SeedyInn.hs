module LordOfTheRings.Location.Cards.SeedyInn (seedyInn) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Spy))

newtype SeedyInn = SeedyInn LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

seedyInn :: LocationCard SeedyInn
seedyInn = location SeedyInn Cards.seedyInn

-- | Travel: Search the encounter deck and discard pile for a Spy enemy and add it to the
-- staging area. Shuffle the encounter deck.
-- Shadow: Raise the defending player's threat by X, where X is the amount of damage dealt
-- by this attack.
instance RunMessage SeedyInn where
  runMessage msg l@(SeedyInn attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (CardMatchAll [CardWithType EnemyType, CardWithTrait Spy])
      SeedyInn <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      attackModifier attrs eid (AttackDamageRaisesThreatOf pid)
      pure l
    _ -> SeedyInn <$> runMessage msg attrs
