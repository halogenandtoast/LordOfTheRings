module LordOfTheRings.Quest.Cards.TheTrialOfPerseverance (theTrialOfPerseverance) where

import LordOfTheRings.Card (HasCardDef (..), toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers
import LordOfTheRings.Trait (Trait (Barrow, Guardian, Key))

newtype TheTrialOfPerseverance = TheTrialOfPerseverance QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTrialOfPerseverance :: QuestCard TheTrialOfPerseverance
theTrialOfPerseverance = quest TheTrialOfPerseverance Cards.theTrialOfPerseverance

-- | Guardian enemies cannot be damaged or leave play except by quest effect: this stage's
-- own, once the players control the current trial's Key.
instance HasModifiersFor TheTrialOfPerseverance where
  getModifiersFor (TheTrialOfPerseverance a) = do
    claimed <- currentKeyControlled
    unless claimed do
      guardians <- select (EnemyWithTrait Guardian)
      for_ guardians \e -> modified a e [CannotTakeDamage, CannotLeavePlay]

-- | When Revealed: Randomly choose 1 of the remaining set aside Barrow locations and add it
-- to the staging area. Then, randomly choose 1 of the remaining set aside Key objectives and
-- attach it to that Barrow location. Find the set aside Guardian enemy that shares a Trait
-- with the Key objective in play and reveal it, adding it to the staging area.
-- When the players control the current trial's Key objective, add all Guardian enemies in
-- play to the victory display and immediately advance to a stage 2A of the players'
-- choice. If all stage 2 quests have been defeated, advance to stage 3A, instead.
instance RunMessage TheTrialOfPerseverance where
  runMessage msg q@(TheTrialOfPerseverance attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheTrialOfPerseverance <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      barrow <- randomSetAside Barrow
      key <- randomSetAside Key
      for_ barrow \b -> do
        addSetAsideToStaging (CardWithCode $ toCardCode b)
        for_ key \k -> do
          setCurrentKey k
          attachSetAsideKey k b
      for_ key \k -> setAsideFor Guardian (toCardDef k) >>= traverse_ revealSetAside
      pure q
    _ -> TheTrialOfPerseverance <$> runTrial (traverse_ addToVictoryDisplay =<< select (EnemyWithTrait Guardian)) msg attrs
