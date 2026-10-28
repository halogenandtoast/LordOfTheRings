module LordOfTheRings.Quest.Cards.ProtectTheVillagers (protectTheVillagers) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (objectiveOf)

newtype ProtectTheVillagers = ProtectTheVillagers QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

protectTheVillagers :: QuestCard ProtectTheVillagers
protectTheVillagers = quest ProtectTheVillagers Cards.protectTheVillagers

-- | If an attack goes undefended, discard X villager tokens from Rescued Villagers instead
-- of damaging a hero. X is the amount of damage that would have been dealt. (Rescued
-- Villagers takes it, discarding a villager for each damage.)
instance HasModifiersFor ProtectTheVillagers where
  getModifiersFor (ProtectTheVillagers a) = when a.flipped do
    rescued <- objectiveOf Objectives.rescuedVillagers
    players <- getRemainingPlayers
    for_ rescued \o -> for_ players \p -> modified a (PlayerTarget p) [UndefendedDamageTo (ObjectiveTarget o)]

-- | When Revealed: Add Ghulat to the staging area.
-- When the players defeat this stage, end the game. Compare the number of tokens on Rescued
-- Villagers to the number of tokens on Dead Villagers (see the scenario).
-- (Ghulat: While Ghulat is in play, the game cannot end.)
instance RunMessage ProtectTheVillagers where
  runMessage msg q@(ProtectTheVillagers attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      ProtectTheVillagers <$> runBlockedStage ghulat msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.ghulat)
      pure q
    _ -> ProtectTheVillagers <$> runBlockedStage ghulat msg attrs
   where
    ghulat = EnemyWithCardCode (toCardCode Enemies.ghulat)
