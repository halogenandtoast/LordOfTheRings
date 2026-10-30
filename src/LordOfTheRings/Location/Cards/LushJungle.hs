module LordOfTheRings.Location.Cards.LushJungle (lushJungle) where

import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

-- | Uncharted, under a Lost Island side.
newtype LushJungle = LushJungle LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

lushJungle :: LocationCard LushJungle
lushJungle = location LushJungle Cards.lushJungle

-- | While Lush Jungle is in the staging area, characters get -1 attack and lose ranged.
instance HasModifiersFor LushJungle where
  getModifiersFor (LushJungle a) = when (a.placement == StagingArea) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [AttackModifier (-1), LoseKeyword Ranged]

-- | Forced: After Lush Jungle is flipped over while active, return it to the staging area
-- unless each player exhausts each ready hero he controls.
instance HasAbilities LushJungle where
  getAbilities (LushJungle a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage LushJungle where
  runMessage msg l@(LushJungle a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      heroes <- select ReadyHero
      chooseOneM lead do
        questionLabeled "lushJungle.choose"
        causedBy a
        labeled "lushJungle.exhaust" $ traverse_ exhaust heroes
        labeled "lushJungle.return" $ placeLocation a.id StagingArea
      pure l
    _ -> LushJungle <$> runMessage msg a
