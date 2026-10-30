module LordOfTheRings.Objective.Cards.HuntingTheOrcs (huntingTheOrcs) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype HuntingTheOrcs = HuntingTheOrcs ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingTheOrcs :: ObjectiveCard HuntingTheOrcs
huntingTheOrcs = objective HuntingTheOrcs Cards.huntingTheOrcs

-- | Forced: After an enemy is defeated, place 1 resource token here. Then if there are at
-- least X resource tokens here, flip Hunting the Orcs and place each resource token here on
-- Savage Counter-attack. After that, advance to stage 2. X is 3 more than the number of
-- players in the game.
instance HasAbilities HuntingTheOrcs where
  getAbilities (HuntingTheOrcs a) = [mkAbility a 1 $ forced $ EnemyDefeatedWindow After]

-- | Savage Counter-attack, its other side, comes into play with the tokens, then the
-- players advance: its own "after it enters play, advance to stage 2A".
instance RunMessage HuntingTheOrcs where
  runMessage msg o@(HuntingTheOrcs attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      players <- length <$> getRemainingPlayers
      let n = countTokens Resource (objectiveTokens attrs)
      when (n >= players + 3) do
        savage <- genEncounterCard Cards.savageCounterattack
        removeFromGame attrs
        addToStaging savage
        placeResources attrs (CardIdTarget savage.id) n
        mains <- getMainQuests
        for_ [qid | QuestTarget qid <- mains] completeQuest
      pure o
    _ -> HuntingTheOrcs <$> runMessage msg attrs
