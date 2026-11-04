module LordOfTheRings.Quest.Cards.BattleInTheDale (battleInTheDale) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (runBattleStage)

newtype BattleInTheDale = BattleInTheDale QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Battle, on its card def.
battleInTheDale :: QuestCard BattleInTheDale
battleInTheDale = quest BattleInTheDale Cards.battleInTheDale

-- | Forced: If there is no progress on this stage at the end of the round, the first player
-- adds the topmost enemy in the discard pile to the staging area, if able.
instance HasAbilities BattleInTheDale where
  getAbilities (BattleInTheDale a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped, a.progress == 0]

-- | Players cannot advance from this stage unless stage 2 and stage 4 are complete (see
-- 'runBattleStage').
instance RunMessage BattleInTheDale where
  runMessage msg q@(BattleInTheDale attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discarded <- getEncounterDiscard
      traverse_ addToStagingFromDiscard $ take 1 [c | c <- discarded, cdCardType (toCardDef c) == EnemyType]
      pure q
    _ -> BattleInTheDale <$> runBattleStage msg attrs
