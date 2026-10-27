module LordOfTheRings.Quest.Cards.ApproachingRivendell (approachingRivendell) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ApproachingRivendell = ApproachingRivendell QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

approachingRivendell :: QuestCard ApproachingRivendell
approachingRivendell = quest ApproachingRivendell Cards.approachingRivendell

-- | Characters cannot be healed.
-- If the players defeat this stage, they have won the game.
instance HasModifiersFor ApproachingRivendell where
  getModifiersFor (ApproachingRivendell a) = do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [CannotBeHealed]

-- | When Revealed: Reveal 1 card from the encounter deck per player, and add it to the
-- staging area.
instance RunMessage ApproachingRivendell where
  runMessage msg q@(ApproachingRivendell attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      ApproachingRivendell <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer $ const revealEncounterCard
      pure q
    _ -> ApproachingRivendell <$> runMessage msg attrs
