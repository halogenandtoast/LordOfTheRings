module LordOfTheRings.Quest.Cards.CursedMists (cursedMists) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Ship))

newtype CursedMists = CursedMists QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedMists :: QuestCard CursedMists
cursedMists = quest CursedMists Cards.cursedMists

-- | Sailing. While you are off-course, each non-Ship ally gets -2 willpower.
instance HasModifiersFor CursedMists where
  getModifiersFor (CursedMists a) = when a.flipped do
    whenM isOffCourse do
      allies <- select (not_ $ AllyWithTrait Ship)
      for_ allies \al -> modified a al [WillpowerModifier (-2)]

-- | When Revealed: Search the encounter deck and discard pile for 1 copy of Fog Bank and add
-- it to the staging area. Shuffle the encounter deck.
instance RunMessage CursedMists where
  runMessage msg q@(CursedMists attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      CursedMists <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Locations.fogBank)
      pure q
    _ -> CursedMists <$> runMessage msg attrs
