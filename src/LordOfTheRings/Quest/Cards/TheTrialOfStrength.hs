module LordOfTheRings.Quest.Cards.TheTrialOfStrength (theTrialOfStrength) where

import LordOfTheRings.Card (HasCardDef (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers
import LordOfTheRings.Trait (Trait (Barrow, Guardian, Key))

newtype TheTrialOfStrength = TheTrialOfStrength QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTrialOfStrength :: QuestCard TheTrialOfStrength
theTrialOfStrength = quest TheTrialOfStrength Cards.theTrialOfStrength

-- | Barrow locations cannot be the active location and are immune to player card effects.
instance HasModifiersFor TheTrialOfStrength where
  getModifiersFor (TheTrialOfStrength a) = do
    barrows <- select (LocationWithTrait Barrow)
    for_ barrows \l -> modified a l [CannotBeTraveledTo, ImmuneToPlayerCardEffects]

-- | When Revealed: Randomly choose 1 of the remaining set aside Guardian enemies and 1 of
-- the remaining set aside Barrow locations, reveal them, and add them to the staging area.
-- Find the set aside Key objective that shares a Trait with the just revealed Guardian
-- enemy and attach it to that enemy.
-- When the players control the current trial's Key objective, add all Barrow locations in
-- play to the victory display and immediately advance to a stage 2A of the players'
-- choice. If all stage 2 quests have been defeated, advance to stage 3A, instead.
instance RunMessage TheTrialOfStrength where
  runMessage msg q@(TheTrialOfStrength attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheTrialOfStrength <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      guardian <- randomSetAside Guardian
      barrow <- randomSetAside Barrow
      traverse_ revealSetAside guardian
      traverse_ revealSetAside barrow
      for_ guardian \g -> setAsideFor Key (toCardDef g) >>= traverse_ \key -> do
        setCurrentKey key
        attachSetAsideKey key g
      pure q
    _ -> TheTrialOfStrength <$> runTrial (traverse_ addToVictoryDisplay =<< select (LocationWithTrait Barrow)) msg attrs
