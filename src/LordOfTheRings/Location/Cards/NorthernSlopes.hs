module LordOfTheRings.Location.Cards.NorthernSlopes (northernSlopes) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (Goblin))

newtype NorthernSlopes = NorthernSlopes LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

northernSlopes :: LocationCard NorthernSlopes
northernSlopes = location NorthernSlopes Cards.northernSlopes

-- | While Northern Slopes is in the staging area, each Goblin enemy gets +1 attack and +1
-- defense.
instance HasModifiersFor NorthernSlopes where
  getModifiersFor (NorthernSlopes a) =
    when (a.placement == StagingArea) do
      goblins <- select (EnemyWithTrait Goblin)
      for_ goblins \g -> modified a g [AttackModifier 1, DefenseModifier 1]

-- | While Northern Slopes is the active location, it gains: "Forced: When a character leaves
-- play, remove 1 progress from the current quest, if able." Answered as the character's
-- leaving play opens its window.
instance HasAbilities NorthernSlopes where
  getAbilities (NorthernSlopes a) =
    [mkAbility a 1 $ forced $ CharacterLeftPlayWindow After AnyCard | a.placement == ActiveLocation]

instance RunMessage NorthernSlopes where
  runMessage msg l@(NorthernSlopes a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeProgressFromQuest a 1
      pure l
    _ -> NorthernSlopes <$> runMessage msg a
