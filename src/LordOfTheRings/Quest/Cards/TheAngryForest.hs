module LordOfTheRings.Quest.Cards.TheAngryForest (theAngryForest) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (mugashOnHero, revealFromDiscard)

newtype TheAngryForest = TheAngryForest QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theAngryForest :: QuestCard TheAngryForest
theAngryForest = quest TheAngryForest Cards.theAngryForest

-- | Forced: After the last time counter is removed from this stage, discard the top 5 cards
-- of the encounter deck. Each player must choose an enemy or objective in the discard pile,
-- reveal it and add it to the staging area. Place 3 time counters on this stage.
instance HasAbilities TheAngryForest where
  getAbilities (TheAngryForest a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Forced: After placing progress here, if there is at least 6 progress on this stage and
-- Mugash is attached to a hero, advance to stage 2A.
instance RunMessage TheAngryForest where
  runMessage msg q@(TheAngryForest attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 5
      eachPlayer revealFromDiscard
      placeTimeCounters attrs attrs 3
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | n > 0, attrs.progress + n >= 6 -> do
      onHero <- mugashOnHero
      when onHero do
        stage2 <- genEncounterCard Cards.escapeFromFangorn
        resetToStage stage2 []
      TheAngryForest <$> runMessage msg attrs
    _ -> TheAngryForest <$> runMessage msg attrs
