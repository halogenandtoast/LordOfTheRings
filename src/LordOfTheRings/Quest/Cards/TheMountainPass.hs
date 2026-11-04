module LordOfTheRings.Quest.Cards.TheMountainPass (theMountainPass) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheMountainPass = TheMountainPass QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMountainPass :: QuestCard TheMountainPass
theMountainPass = quest TheMountainPass Cards.theMountainPass

-- | When Revealed: Search the encounter deck for 1 copy of Stone-giant and add it to the
-- staging area. Then, shuffle the encounter deck. Reveal 1 card per player from the
-- encounter deck and add it to the staging area.
instance RunMessage TheMountainPass where
  runMessage msg q@(TheMountainPass attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheMountainPass <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Enemies.stoneGiant)
      eachPlayer $ const revealEncounterCard
      pure q
    _ -> TheMountainPass <$> runMessage msg attrs
