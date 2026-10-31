module LordOfTheRings.Quest.Cards.EscapeFromFangorn (escapeFromFangorn) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (canDefeatStage, huorn, shuffleMugashIntoEncounterDeck)

newtype EscapeFromFangorn = EscapeFromFangorn QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromFangorn :: QuestCard EscapeFromFangorn
escapeFromFangorn = quest EscapeFromFangorn Cards.escapeFromFangorn

-- | Forced: After the last time counter is removed from this stage, shuffle Mugash into the
-- encounter deck and advance to stage 3A.
instance HasAbilities EscapeFromFangorn where
  getAbilities (EscapeFromFangorn a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | When Revealed: Each player searches the encounter deck and discard pile for a Huorn
-- enemy and adds it to the staging area. Shuffle the encounter deck.
-- The players cannot defeat this stage unless Mugash is attached to a hero. If the players
-- defeat this stage, they win the game: it is the last in the quest deck, whether reached
-- from stage 1 or back from stage 3.
instance RunMessage EscapeFromFangorn where
  runMessage msg q@(EscapeFromFangorn attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      EscapeFromFangorn <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscard pid attrs huorn
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleMugashIntoEncounterDeck
      stage3 <- genEncounterCard Cards.theAngryForest
      resetToStage stage3 []
      pure q
    _ -> EscapeFromFangorn <$> runConditionalStage canDefeatStage msg attrs
