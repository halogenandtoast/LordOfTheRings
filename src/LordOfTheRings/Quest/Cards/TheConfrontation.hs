module LordOfTheRings.Quest.Cards.TheConfrontation (theConfrontation) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (shuffleUnderworldIntoEncounterDeck)
import LordOfTheRings.Trait (Trait (Villain))

newtype TheConfrontation = TheConfrontation QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theConfrontation :: QuestCard TheConfrontation
theConfrontation = quest TheConfrontation Cards.theConfrontation

-- | When Revealed: Reveal the set aside Villain card and add it to the staging area.
-- When Revealed: Shuffle the underworld deck into the encounter deck.
-- The players cannot defeat this stage while a Villain is in play. If this stage is
-- defeated, the players have won the game.
instance RunMessage TheConfrontation where
  runMessage msg q@(TheConfrontation attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheConfrontation <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (CardWithTrait Villain)
      shuffleUnderworldIntoEncounterDeck
      pure q
    _ -> TheConfrontation <$> runBlockedStage (EnemyWithTrait Villain) msg attrs
