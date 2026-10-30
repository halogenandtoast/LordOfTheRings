module LordOfTheRings.Quest.Cards.IntoTheStorm (intoTheStorm) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype IntoTheStorm = IntoTheStorm QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheStorm :: QuestCard IntoTheStorm
intoTheStorm = quest IntoTheStorm Cards.intoTheStorm

-- | Sailing. Forced: At the end of the quest phase, if you are off-course, each player must
-- distribute 5 damage among characters he controls.
instance HasAbilities IntoTheStorm where
  getAbilities (IntoTheStorm a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.flipped]

-- | When Revealed: Search the encounter deck and discard pile for 1 copy of Waterspout and
-- add it to the staging area. Shuffle the encounter deck.
instance RunMessage IntoTheStorm where
  runMessage msg q@(IntoTheStorm attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      IntoTheStorm <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Locations.waterspout)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM isOffCourse $ eachPlayer \p -> dealDamageDivided attrs p 5
      pure q
    _ -> IntoTheStorm <$> runMessage msg attrs
