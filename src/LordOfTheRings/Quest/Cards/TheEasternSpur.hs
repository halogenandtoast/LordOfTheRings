module LordOfTheRings.Quest.Cards.TheEasternSpur (theEasternSpur) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (runBattleStage)

newtype TheEasternSpur = TheEasternSpur QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEasternSpur :: QuestCard TheEasternSpur
theEasternSpur = quest TheEasternSpur Cards.theEasternSpur

-- | Forced: If there is no progress on this stage at the end of the round, each player must
-- discard 1 random card from his hand.
instance HasAbilities TheEasternSpur where
  getAbilities (TheEasternSpur a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped, a.progress == 0]

-- | Players cannot advance from this stage unless stage 3 and stage 4 are complete (see
-- 'runBattleStage').
instance RunMessage TheEasternSpur where
  runMessage msg q@(TheEasternSpur attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> discardRandomFromHand pid 1
      pure q
    _ -> TheEasternSpur <$> runBattleStage msg attrs
