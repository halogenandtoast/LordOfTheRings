module LordOfTheRings.Quest.Cards.TryingToBlendIn (tryingToBlendIn) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers

newtype TryingToBlendIn = TryingToBlendIn QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

tryingToBlendIn :: QuestCard TryingToBlendIn
tryingToBlendIn = quest TryingToBlendIn Cards.tryingToBlendIn

-- | City Guard cannot have more than 10 damage, and it gets -5 engagement cost for each
-- resource here.
instance HasModifiersFor TryingToBlendIn where
  getModifiersFor (TryingToBlendIn a) = cityGuardStageModifiers a 10

-- | Forced: At the end of the round, place 1 resource here.
instance HasAbilities TryingToBlendIn where
  getAbilities (TryingToBlendIn a) = cityGuardStageAbilities a

-- | When Revealed: Each player must choose: either raise your threat by 5, or reveal an
-- encounter card.
instance RunMessage TryingToBlendIn where
  runMessage msg q@(TryingToBlendIn attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TryingToBlendIn <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeled "tryingToBlendIn.choose"
        causedBy attrs
        labeled "tryingToBlendIn.threat" $ encounterRaisesThreat p 5
        labeled "tryingToBlendIn.reveal" revealEncounterCard
      pure q
    _ -> TryingToBlendIn <$> runCityGuardStage msg attrs
