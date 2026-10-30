module LordOfTheRings.Quest.Cards.TheStarsGuidance (theStarsGuidance) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Ocean))

newtype TheStarsGuidance = TheStarsGuidance QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theStarsGuidance :: QuestCard TheStarsGuidance
theStarsGuidance = quest TheStarsGuidance Cards.theStarsGuidance

-- | Sailing. While you are off-course, each Ocean location gets +1 threat.
instance HasModifiersFor TheStarsGuidance where
  getModifiersFor (TheStarsGuidance a) = when a.flipped do
    whenM isOffCourse do
      oceans <- select (LocationWithTrait Ocean)
      for_ oceans \l -> modified a l [ThreatModifier 1]

-- | When Revealed: Search the encounter deck and discard pile for 1 copy of Starlit Sea and
-- add it to the staging area. Shuffle the encounter deck.
instance RunMessage TheStarsGuidance where
  runMessage msg q@(TheStarsGuidance attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheStarsGuidance <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Locations.starlitSea)
      pure q
    _ -> TheStarsGuidance <$> runMessage msg attrs
