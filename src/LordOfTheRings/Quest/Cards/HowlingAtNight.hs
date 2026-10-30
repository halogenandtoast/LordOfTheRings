module LordOfTheRings.Quest.Cards.HowlingAtNight (howlingAtNight) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (discardUntilWargs)

newtype HowlingAtNight = HowlingAtNight QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

howlingAtNight :: QuestCard HowlingAtNight
howlingAtNight = quest HowlingAtNight Cards.howlingAtNight

-- | Forced: When it becomes Day, each player draws 1 card.
-- Forced: When it becomes Night, discard 1 non-objective ally from play (discard 2
-- non-objective allies instead if there are 3 or more players in the game).
instance HasAbilities HowlingAtNight where
  getAbilities (HowlingAtNight a) =
    guard a.flipped
      *> [ mkAbility a 1 $ forced $ BecomesWindow Daytime
         , mkAbility a 2 $ forced $ BecomesWindow Nighttime
         ]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
-- cards from the top of the encounter deck until X Warg enemies are discarded. X is the
-- number of players in the game. Add each discarded enemy to the staging area.
-- The first player chooses the allies discarded, one at a time.
instance RunMessage HowlingAtNight where
  runMessage msg q@(HowlingAtNight attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      HowlingAtNight <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardUntilWargs . length =<< getRemainingPlayers
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> drawCards attrs p 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      players <- length <$> getRemainingPlayers
      doStep (if players >= 3 then 2 else 1) msg
      pure q
    DoStep n inner@(UseCardAbility _ (isSource attrs -> True) 2 _) | n > 0 -> do
      lead <- getLead
      allies <- select (NotAlly ObjectiveAlly)
      chooseOrRunOneM lead do
        questionLabeled "howlingAtNight.discardAlly"
        causedBy attrs
        targets allies discard
      doStep (n - 1) inner
      pure q
    _ -> HowlingAtNight <$> runMessage msg attrs
