module LordOfTheRings.Location.Cards.LabyrinthineWoods (labyrinthineWoods) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Creature))

newtype LabyrinthineWoods = LabyrinthineWoods LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

labyrinthineWoods :: LocationCard LabyrinthineWoods
labyrinthineWoods = location LabyrinthineWoods Cards.labyrinthineWoods

-- | While Labyrinthine Woods is in the staging area, Creature enemies get +1 threat and +1
-- attack.
instance HasModifiersFor LabyrinthineWoods where
  getModifiersFor (LabyrinthineWoods a) = when (a.placement == StagingArea) do
    creatures <- select (EnemyWithTrait Creature)
    for_ creatures \c -> modified a c [ThreatModifier 1, AttackModifier 1]

-- | Shadow: After this attack, attacking enemy engages the next player and makes an
-- additional attack.
instance RunMessage LabyrinthineWoods where
  runMessage msg l@(LabyrinthineWoods a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      getNextPlayer pid >>= traverse_ \next ->
        afterAttack eid =<< capture do
          engageEnemy next eid
          immediateAttack next eid
      pure l
    _ -> LabyrinthineWoods <$> runMessage msg a
