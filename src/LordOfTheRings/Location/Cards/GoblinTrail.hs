module LordOfTheRings.Location.Cards.GoblinTrail (goblinTrail) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GoblinTrail = GoblinTrail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTrail :: LocationCard GoblinTrail
goblinTrail = location GoblinTrail Cards.goblinTrail

instance HasAbilities GoblinTrail where
  getAbilities (GoblinTrail a) = [mkAbility a 1 $ response $ ExploredWindow After a.id]

instance RunMessage GoblinTrail where
  runMessage msg l@(GoblinTrail a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      GoblinTrail <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ (\q -> placeProgress a q 6) =<< getCurrentQuest
      pure l
    _ -> GoblinTrail <$> runMessage msg a
