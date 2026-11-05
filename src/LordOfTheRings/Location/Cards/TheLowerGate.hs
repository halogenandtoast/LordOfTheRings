module LordOfTheRings.Location.Cards.TheLowerGate (theLowerGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Creature, Goblin))

newtype TheLowerGate = TheLowerGate LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLowerGate :: LocationCard TheLowerGate
theLowerGate = location TheLowerGate Cards.theLowerGate

instance HasModifiersFor TheLowerGate where
  getModifiersFor (TheLowerGate a) = do
    current <- getCurrentQuest
    for_ current \q -> modified a q [ProgressPerRoundLimit 4]
    creatures <- select (EnemyWithTrait Creature)
    goblins <- select (EnemyWithTrait Goblin)
    for_ creatures \e -> modified a e [AttackModifier 1]
    for_ goblins \e -> modified a e [DefenseModifier 1]

instance RunMessage TheLowerGate where
  runMessage msg (TheLowerGate attrs) = TheLowerGate <$> runMessage msg attrs
