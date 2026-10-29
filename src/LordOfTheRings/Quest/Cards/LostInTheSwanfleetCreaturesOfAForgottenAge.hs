module LordOfTheRings.Quest.Cards.LostInTheSwanfleetCreaturesOfAForgottenAge (lostInTheSwanfleetCreaturesOfAForgottenAge) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (addMarshDweller, runWanderingStage, wanderingStage)

newtype LostInTheSwanfleetCreaturesOfAForgottenAge = LostInTheSwanfleetCreaturesOfAForgottenAge QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheSwanfleetCreaturesOfAForgottenAge :: QuestCard LostInTheSwanfleetCreaturesOfAForgottenAge
lostInTheSwanfleetCreaturesOfAForgottenAge =
  quest LostInTheSwanfleetCreaturesOfAForgottenAge Cards.lostInTheSwanfleetCreaturesOfAForgottenAge

instance HasAbilities LostInTheSwanfleetCreaturesOfAForgottenAge where
  getAbilities (LostInTheSwanfleetCreaturesOfAForgottenAge a) = wanderingStage a

-- | Each enemy in the staging area gets -20 engagement cost.
instance HasModifiersFor LostInTheSwanfleetCreaturesOfAForgottenAge where
  getModifiersFor (LostInTheSwanfleetCreaturesOfAForgottenAge a) = when a.flipped do
    enemies <- select EnemyInStagingArea
    for_ enemies \e -> modified a e [EngagementCostModifier (-20)]

-- | When Revealed: Raise each player's threat by 1. Add Ancient Marsh-dweller to the
-- staging area (from out of play or engaged with a player) and heal all damage from it.
instance RunMessage LostInTheSwanfleetCreaturesOfAForgottenAge where
  runMessage msg q@(LostInTheSwanfleetCreaturesOfAForgottenAge attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      addMarshDweller
      pure q
    _ -> LostInTheSwanfleetCreaturesOfAForgottenAge <$> runWanderingStage msg attrs
