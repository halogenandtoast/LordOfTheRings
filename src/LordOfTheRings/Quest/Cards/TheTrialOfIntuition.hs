module LordOfTheRings.Quest.Cards.TheTrialOfIntuition (theTrialOfIntuition) where

import LordOfTheRings.Card (HasCardDef (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers
import LordOfTheRings.Trait (Trait (Barrow, Guardian, Key))

newtype TheTrialOfIntuition = TheTrialOfIntuition QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTrialOfIntuition :: QuestCard TheTrialOfIntuition
theTrialOfIntuition = quest TheTrialOfIntuition Cards.theTrialOfIntuition

-- | When Revealed: Randomly choose 1 of the remaining set aside Guardian enemies and 1 of
-- the remaining set aside Barrow locations, reveal them, and add them to the staging area.
-- Shuffle the encounter discard pile into the encounter deck. Find the set aside Key
-- objective that shares a Trait with the just revealed Guardian enemy and shuffle it into
-- the bottom 10 cards of the encounter deck.
-- Any time players would place progress on this quest, discard an equal number of cards
-- from the encounter deck instead. (Progress is placed on the active location before
-- triggering this effect.)
-- When the players control the current trial's Key objective, add all Guardian enemies and
-- Barrow locations in play to the victory display and immediately advance to a stage 2A of
-- the players' choice. If all stage 2 quests have been defeated, advance to stage 3A,
-- instead.
instance RunMessage TheTrialOfIntuition where
  runMessage msg q@(TheTrialOfIntuition attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheTrialOfIntuition <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      guardian <- randomSetAside Guardian
      barrow <- randomSetAside Barrow
      traverse_ revealSetAside guardian
      traverse_ revealSetAside barrow
      shuffleEncounterDiscardIntoDeck AnyCard
      for_ guardian \g -> setAsideFor Key (toCardDef g) >>= traverse_ \key -> do
        setCurrentKey key
        shuffleIntoBottomTen key
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      discardTopOfEncounterDeck attrs n
      pure q
    _ -> TheTrialOfIntuition <$> runTrial onDefeat msg attrs
   where
    onDefeat = do
      traverse_ addToVictoryDisplay =<< select (EnemyWithTrait Guardian)
      traverse_ addToVictoryDisplay =<< select (LocationWithTrait Barrow)
